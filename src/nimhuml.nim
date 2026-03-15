## nimhuml - A Nim implementation of HUML (Human-Oriented Markup Language)
##
## HUML is a machine-readable markup language with a focus on readability by humans.
## It borrows YAML's visual appearance, but avoids its complexities and ambiguities.

import std/[json, strutils, math, strformat, tables]

const
  SupportedVersion* = "v0.2.0"
  MultilineIndent = 2

type
  HumlError* = object of CatchableError
    ## Base exception for HUML errors.

  HumlParseError* = object of HumlError
    ## Exception raised when parsing fails.
    line*: int

  Parser = object
    data: string
    pos: int
    line: int

# Forward declarations
proc parseDocument(p: var Parser): JsonNode
proc parseValue(p: var Parser, keyIndent: int): JsonNode
proc parseMultilineDict(p: var Parser, indent: int): JsonNode
proc parseMultilineList(p: var Parser, indent: int): JsonNode
proc parseInlineDict(p: var Parser): JsonNode
proc parseInlineList(p: var Parser): JsonNode
proc parseVector(p: var Parser, indent: int): JsonNode
proc parseKey(p: var Parser): string
proc parseString(p: var Parser): string
proc parseMultilineString(p: var Parser, keyIndent: int): string
proc parseNumber(p: var Parser): JsonNode
proc writeValue(output: var string, value: JsonNode, indent: int)

# ---------------------------------------------------------------------------
# Parser helpers
# ---------------------------------------------------------------------------

proc newHumlParseError(msg: string, line: int): ref HumlParseError =
  result = newException(HumlParseError, &"line {line}: {msg}")
  result.line = line

proc newParser(data: string): Parser =
  Parser(data: data.replace("\r", ""), pos: 0, line: 1)

proc error(p: Parser, msg: string): ref HumlParseError =
  newHumlParseError(msg, p.line)

proc done(p: Parser): bool {.inline.} =
  p.pos >= p.data.len

proc peek(p: Parser, offset: int = 0): char {.inline.} =
  let pos = p.pos + offset
  if pos >= 0 and pos < p.data.len:
    p.data[pos]
  else:
    '\0'

proc peekString(p: Parser, s: string): bool {.inline.} =
  if p.pos + s.len > p.data.len:
    return false
  for i in 0 ..< s.len:
    if p.data[p.pos + i] != s[i]:
      return false
  true

proc advance(p: var Parser, n: int = 1) {.inline.} =
  p.pos += n

proc skipSpaces(p: var Parser) =
  while not p.done and p.data[p.pos] == ' ':
    p.pos += 1

proc getIndent(p: Parser): int =
  # Find line start
  var start = p.pos
  while start > 0 and p.data[start - 1] != '\n':
    start -= 1
  # Count spaces from line start
  var indent = 0
  while start + indent < p.data.len and p.data[start + indent] == ' ':
    indent += 1
  indent

proc lineStart(p: Parser): int =
  var start = p.pos
  while start > 0 and p.data[start - 1] != '\n':
    start -= 1
  start

proc consumeLine(p: var Parser) =
  let contentStart = p.pos
  p.skipSpaces()

  if p.done or p.data[p.pos] == '\n':
    # Check for trailing spaces on empty line
    if p.pos > contentStart:
      raise p.error("trailing spaces are not allowed")
  elif p.data[p.pos] == '#':
    # Handle inline comment
    if p.pos == contentStart and p.getIndent() != p.pos - p.lineStart():
      raise p.error("a value must be separated from an inline comment by a space")
    p.pos += 1 # Consume '#'
    if not p.done and p.data[p.pos] notin {' ', '\n'}:
      raise p.error("comment hash '#' must be followed by a space")
    # Skip to end of line
    while not p.done and p.data[p.pos] != '\n':
      p.pos += 1
    # Check for trailing spaces in comment
    if p.pos > 0 and p.data[p.pos - 1] == ' ':
      raise p.error("trailing spaces are not allowed")
  else:
    raise p.error("unexpected content at end of line")

  # Consume newline
  if not p.done and p.data[p.pos] == '\n':
    p.pos += 1
    p.line += 1

proc consumeLineRaw(p: var Parser): string =
  let start = p.pos
  while not p.done and p.data[p.pos] != '\n':
    p.pos += 1
  result = p.data[start ..< p.pos]
  if not p.done:
    p.pos += 1
    p.line += 1

proc skipBlankLines(p: var Parser) =
  while not p.done:
    let lineStart = p.pos
    p.skipSpaces()

    if p.done:
      if p.pos > lineStart:
        raise p.error("trailing spaces are not allowed")
      return

    if p.data[p.pos] notin {'\n', '#'}:
      return

    # Check for trailing spaces on blank lines
    if p.data[p.pos] == '\n' and p.pos > lineStart:
      raise p.error("trailing spaces are not allowed")

    # Reset and consume the line
    p.pos = lineStart
    p.consumeLine()

proc expectSingleSpace(p: var Parser, context: string) =
  if p.done or p.data[p.pos] != ' ':
    raise p.error(&"expected single space {context}")
  p.advance()
  if not p.done and p.data[p.pos] == ' ':
    raise p.error(&"expected single space {context}, found multiple")

proc expectComma(p: var Parser) =
  p.skipSpaces()
  if p.done or p.data[p.pos] != ',':
    raise p.error("expected a comma in inline collection")
  # No spaces allowed before comma
  if p.pos > 0 and p.data[p.pos - 1] == ' ':
    raise p.error("no spaces allowed before comma")
  p.advance()
  p.expectSingleSpace("after comma")

# ---------------------------------------------------------------------------
# Type determination helpers
# ---------------------------------------------------------------------------

proc isKeyValueLine(p: var Parser): bool =
  let savedPos = p.pos
  try:
    discard p.parseKey()
    result = not p.done and p.data[p.pos] == ':'
  except:
    result = false
  finally:
    p.pos = savedPos

proc isInlineDictRoot(p: var Parser): bool =
  var pos = p.pos
  var hasColon = false
  var hasComma = false
  var hasDoubleColon = false

  while pos < p.data.len and p.data[pos] notin {'\n', '#'}:
    if p.data[pos] == ':':
      if pos + 1 < p.data.len and p.data[pos + 1] == ':':
        hasDoubleColon = true
      hasColon = true
    elif p.data[pos] == ',':
      hasComma = true
    pos += 1

  if not (hasColon and hasComma and not hasDoubleColon):
    return false

  # Check if there's content after this line
  while pos < p.data.len and p.data[pos] != '\n':
    pos += 1
  if pos < p.data.len:
    pos += 1

  # Skip blank lines and comments to see if there's more content
  while pos < p.data.len:
    while pos < p.data.len and p.data[pos] == ' ':
      pos += 1
    if pos >= p.data.len:
      break
    if p.data[pos] == '\n':
      pos += 1
      continue
    if p.data[pos] == '#':
      while pos < p.data.len and p.data[pos] != '\n':
        pos += 1
      if pos < p.data.len:
        pos += 1
      continue
    # Found non-blank, non-comment content
    return false

  true

proc hasCommaOnLine(p: Parser): bool =
  var pos = p.pos
  while pos < p.data.len and p.data[pos] notin {'\n', '#'}:
    if p.data[pos] == ',':
      return true
    if p.data[pos] == ':':
      return false
    pos += 1
  false

proc hasDictPattern(p: Parser): bool =
  var pos = p.pos
  while pos < p.data.len and p.data[pos] notin {'\n', '#'}:
    if p.data[pos] == ':' and (pos + 1 >= p.data.len or p.data[pos + 1] != ':'):
      return true
    pos += 1
  false

proc determineDocType(p: var Parser): string =
  # Check for forbidden root indicators
  if p.peekString("::"):
    raise p.error("'::' indicator not allowed at document root")
  if p.peekString(":") and not p.isKeyValueLine():
    raise p.error("':' indicator not allowed at document root")

  if p.isKeyValueLine():
    return if p.isInlineDictRoot(): "inline_dict" else: "multiline_dict"

  if p.peekString("[]"):
    return "empty_list"
  if p.peekString("{}"):
    return "empty_dict"
  if p.peek() == '-':
    return "multiline_list"
  if p.hasCommaOnLine():
    return "inline_list"

  "scalar"

proc parseByType(p: var Parser, docType: string, indent: int): JsonNode =
  case docType
  of "empty_list":
    p.advance(2)
    p.consumeLine()
    return newJArray()
  of "empty_dict":
    p.advance(2)
    p.consumeLine()
    return newJObject()
  of "inline_dict":
    return p.parseInlineDict()
  of "inline_list":
    return p.parseInlineList()
  of "multiline_dict":
    return p.parseMultilineDict(indent)
  of "multiline_list":
    return p.parseMultilineList(indent)
  of "scalar":
    result = p.parseValue(indent)
    p.consumeLine()
    return result
  else:
    raise p.error(&"internal error: unknown type '{docType}'")

# ---------------------------------------------------------------------------
# Key parsing
# ---------------------------------------------------------------------------

proc parseKey(p: var Parser): string =
  p.skipSpaces()

  if p.peek() == '"':
    return p.parseString()

  # Bare key - must start with letter
  if p.peek() == '\0' or not p.peek().isAlphaAscii():
    raise p.error("expected a key")

  let start = p.pos
  while not p.done and (p.data[p.pos].isAlphaNumeric() or p.data[p.pos] in {'-', '_'}):
    p.pos += 1

  p.data[start ..< p.pos]

# ---------------------------------------------------------------------------
# String parsing
# ---------------------------------------------------------------------------

proc parseString(p: var Parser): string =
  p.advance() # Skip opening quote
  var parts: seq[string] = @[]

  while not p.done:
    let c = p.data[p.pos]

    if c == '"':
      p.advance()
      return parts.join("")

    if c == '\n':
      raise p.error("newlines not allowed in single-line strings")

    if c == '\\':
      p.advance()
      if p.done:
        raise p.error("incomplete escape sequence")
      let esc = p.data[p.pos]
      case esc
      of '"': parts.add("\"")
      of '\\': parts.add("\\")
      of '/': parts.add("/")
      of 'n': parts.add("\n")
      of 't': parts.add("\t")
      of 'r': parts.add("\r")
      of 'b': parts.add("\b")
      of 'f': parts.add("\f")
      else:
        raise p.error(&"invalid escape character '\\{esc}'")
    else:
      parts.add($c)

    p.advance()

  raise p.error("unclosed string")

proc parseMultilineString(p: var Parser, keyIndent: int): string =
  p.advance(3) # Skip """
  p.consumeLine()

  var lines: seq[string] = @[]

  while not p.done:
    let lineStartPos = p.pos

    # Count indentation
    var indent = 0
    while p.pos < p.data.len and p.data[p.pos] == ' ':
      indent += 1
      p.pos += 1

    # Check for closing delimiter
    if p.peekString("\"\"\""):
      if indent != keyIndent:
        raise p.error(
          &"multiline closing delimiter must be at same indentation as the key ({keyIndent} spaces)")
      p.advance(3)
      p.consumeLine()
      return lines.join("\n")

    # Get line content
    p.pos = lineStartPos
    let lineContent = p.consumeLineRaw()

    # Strip required 2-space indent relative to key
    let reqIndent = keyIndent + MultilineIndent
    if lineContent.len >= reqIndent:
      var allSpaces = true
      for i in 0 ..< reqIndent:
        if lineContent[i] != ' ':
          allSpaces = false
          break
      if allSpaces:
        lines.add(lineContent[reqIndent .. ^1])
      else:
        lines.add(lineContent)
    else:
      lines.add(lineContent)

  raise p.error("unclosed multiline string")

# ---------------------------------------------------------------------------
# Number parsing
# ---------------------------------------------------------------------------

proc isDigit(c: char): bool {.inline.} =
  c in {'0'..'9'}

proc parseIntInBase(s: string, base: int): BiggestInt =
  ## Parse a string as a BiggestInt in the given base.
  result = 0
  for c in s:
    let digit = case c
      of '0'..'9': ord(c) - ord('0')
      of 'a'..'f': ord(c) - ord('a') + 10
      of 'A'..'F': ord(c) - ord('A') + 10
      else: raise newException(ValueError, &"invalid digit '{c}' for base {base}")
    if digit >= base:
      raise newException(ValueError, &"digit '{c}' out of range for base {base}")
    result = result * BiggestInt(base) + BiggestInt(digit)

proc parseBaseNumber(p: var Parser, start: int, base: int, prefix: string): JsonNode =
  p.advance(prefix.len)
  let numStart = p.pos

  let validChars = case base
    of 2: {'0', '1', '_'}
    of 8: {'0'..'7', '_'}
    of 16: {'0'..'9', 'a'..'f', 'A'..'F', '_'}
    else: {'0'..'9', '_'}

  while not p.done and p.data[p.pos] in validChars:
    p.advance()

  if p.pos == numStart:
    raise p.error("invalid number literal, requires digits after prefix")

  let sign = if p.data[start] == '-': -1 else: 1
  let numStr = p.data[numStart ..< p.pos].replace("_", "")

  try:
    let value = parseIntInBase(numStr, base)
    return newJInt(BiggestInt(sign) * value)
  except ValueError:
    raise p.error(&"invalid number: {getCurrentExceptionMsg()}")

proc parseNumber(p: var Parser): JsonNode =
  let start = p.pos

  # Handle sign
  if p.peek() in {'+', '-'}:
    p.advance()

  # Check for special bases
  if p.peekString("0x"):
    return p.parseBaseNumber(start, 16, "0x")
  if p.peekString("0o"):
    return p.parseBaseNumber(start, 8, "0o")
  if p.peekString("0b"):
    return p.parseBaseNumber(start, 2, "0b")

  # Parse decimal number
  var isFloat = false

  while not p.done:
    let c = p.data[p.pos]
    if c in {'0'..'9', '_'}:
      p.advance()
    elif c == '.':
      isFloat = true
      p.advance()
    elif c in {'e', 'E'}:
      isFloat = true
      p.advance()
      if p.peek() in {'+', '-'}:
        p.advance()
    else:
      break

  let numStr = p.data[start ..< p.pos].replace("_", "")

  try:
    if isFloat:
      let floatResult = parseFloat(numStr)
      # Convert to int if it represents an integer value
      if floatResult == floatResult.floor and floatResult == floatResult.ceil: # not NaN
        if 'e' in numStr.toLowerAscii or 'E' in numStr:
          # Parse the exponent
          let parts = numStr.toLowerAscii.split('e')
          if parts.len == 2:
            try:
              let exponent = parseInt(parts[1])
              if exponent >= 0 and abs(floatResult) < 1e15:
                return newJInt(BiggestInt(floatResult))
            except ValueError:
              discard
          return newJFloat(floatResult)
        else:
          # Simple decimal like 0.0, 42.0 -> convert to int
          return newJInt(BiggestInt(floatResult))
      return newJFloat(floatResult)
    else:
      return newJInt(parseBiggestInt(numStr))
  except ValueError:
    raise p.error(&"invalid number: {getCurrentExceptionMsg()}")

# ---------------------------------------------------------------------------
# Value parsing
# ---------------------------------------------------------------------------

proc parseValue(p: var Parser, keyIndent: int): JsonNode =
  if p.done:
    raise p.error("unexpected end of input, expected a value")

  let c = p.data[p.pos]

  # String literals
  if c == '"':
    if p.peekString("\"\"\""):
      return newJString(p.parseMultilineString(keyIndent))
    else:
      return newJString(p.parseString())

  # Keywords
  type KVPair = tuple[keyword: string, value: JsonNode]
  let keywords: seq[KVPair] = @[
    ("true", newJBool(true)),
    ("false", newJBool(false)),
    ("null", newJNull()),
    ("nan", newJFloat(NaN)),
    ("inf", newJFloat(Inf))
  ]

  for kv in keywords:
    if p.peekString(kv.keyword):
      let nextPos = p.pos + kv.keyword.len
      if nextPos >= p.data.len or not (p.data[nextPos].isAlphaNumeric() or p.data[nextPos] == '_'):
        p.advance(kv.keyword.len)
        return kv.value

  # Special numeric values with signs
  if c in {'+', '-'}:
    p.advance()
    if p.peekString("inf"):
      p.advance(3)
      return newJFloat(if c == '+': Inf else: NegInf)
    if not p.done and p.data[p.pos].isDigit:
      p.pos -= 1 # Put sign back for number parser
      return p.parseNumber()
    raise p.error(&"invalid character after '{c}'")

  # Regular numbers
  if c.isDigit:
    return p.parseNumber()

  raise p.error(&"unexpected character '{c}' when parsing value")

# ---------------------------------------------------------------------------
# Collection parsing
# ---------------------------------------------------------------------------

proc skipSpacesBeforeComma(p: var Parser) =
  if not p.done and p.data[p.pos] == ' ':
    var nextPos = p.pos + 1
    while nextPos < p.data.len and p.data[nextPos] == ' ':
      nextPos += 1
    if nextPos < p.data.len and p.data[nextPos] == ',':
      p.skipSpaces()

proc parseMultilineDict(p: var Parser, indent: int): JsonNode =
  result = newJObject()

  while true:
    p.skipBlankLines()
    if p.done or p.getIndent() < indent:
      break
    if p.getIndent() != indent:
      raise p.error(&"bad indent {p.getIndent()}, expected {indent}")

    let key = p.parseKey()
    if result.hasKey(key):
      raise p.error(&"duplicate key '{key}' in dict")

    if not p.done and p.data[p.pos] == ':':
      p.advance()
      if not p.done and p.data[p.pos] == ':':
        # :: indicator - parse vector
        p.advance()
        result[key] = p.parseVector(indent + MultilineIndent)
      else:
        # : indicator - parse value
        p.expectSingleSpace("after ':'")
        let isMultiline = p.peekString("\"\"\"")
        result[key] = p.parseValue(indent)
        if not isMultiline:
          p.consumeLine()
    else:
      raise p.error("expected ':' or '::' after key")

proc parseMultilineList(p: var Parser, indent: int): JsonNode =
  result = newJArray()

  while true:
    p.skipBlankLines()
    if p.done or p.getIndent() < indent:
      break
    if p.getIndent() != indent:
      raise p.error(&"bad indent {p.getIndent()}, expected {indent}")
    if p.data[p.pos] != '-':
      break

    p.advance()
    p.expectSingleSpace("after '-'")

    # Check for nested vector
    if p.peekString("::"):
      p.advance(2)
      result.add(p.parseVector(indent + MultilineIndent))
    else:
      let value = p.parseValue(indent)
      p.consumeLine()
      result.add(value)

proc parseVector(p: var Parser, indent: int): JsonNode =
  let startPos = p.pos
  p.skipSpaces()

  # Check for multiline vector
  if p.done or p.data[p.pos] in {'\n', '#'}:
    p.pos = startPos
    let vectorLine = p.line
    p.consumeLine()

    p.skipBlankLines()

    if p.done or p.getIndent() < indent:
      raise newHumlParseError("ambiguous empty vector after '::'. Use [] or {}.", vectorLine)

    if p.data[p.pos] == '-':
      return p.parseMultilineList(indent)
    else:
      return p.parseMultilineDict(indent)

  # Inline vector - must have exactly one space
  p.pos = startPos
  p.expectSingleSpace("after '::'")

  # Check for empty markers
  if p.peekString("[]"):
    p.advance(2)
    p.consumeLine()
    return newJArray()

  if p.peekString("{}"):
    p.advance(2)
    p.consumeLine()
    return newJObject()

  # Determine if dict or list by scanning for colons
  if p.hasDictPattern():
    return p.parseInlineDict()
  else:
    return p.parseInlineList()

proc parseInlineDict(p: var Parser): JsonNode =
  result = newJObject()
  var isFirst = true

  while not p.done and p.data[p.pos] notin {'\n', '#'}:
    if not isFirst:
      p.expectComma()
    isFirst = false

    let key = p.parseKey()
    if result.hasKey(key):
      raise p.error(&"duplicate key '{key}' in dict")

    if p.done or p.data[p.pos] != ':':
      raise p.error("expected ':' in inline dict")

    p.advance()
    p.expectSingleSpace("in inline dict")

    let value = p.parseValue(0)
    result[key] = value

    p.skipSpacesBeforeComma()

  p.consumeLine()

proc parseInlineList(p: var Parser): JsonNode =
  result = newJArray()
  var isFirst = true

  while not p.done and p.data[p.pos] notin {'\n', '#'}:
    if not isFirst:
      p.expectComma()
    isFirst = false

    result.add(p.parseValue(0))
    p.skipSpacesBeforeComma()

  p.consumeLine()

# ---------------------------------------------------------------------------
# Document parsing
# ---------------------------------------------------------------------------

proc parseDocument(p: var Parser): JsonNode =
  # Check for version directive
  if p.peekString("%HUML"):
    p.advance(5)

    if not p.done and p.data[p.pos] == ' ':
      p.advance()
      let start = p.pos
      while not p.done and p.data[p.pos] notin {' ', '\n', '#'}:
        p.pos += 1

      if p.pos > start:
        let version = p.data[start ..< p.pos]
        if version != SupportedVersion:
          raise p.error(
            &"unsupported version '{version}'. expected '{SupportedVersion}'")

    p.consumeLine()

  p.skipBlankLines()

  if p.done:
    raise p.error("empty document is undefined")

  if p.getIndent() != 0:
    raise p.error("root element must not be indented")

  let docType = p.determineDocType()
  result = p.parseByType(docType, 0)

  p.skipBlankLines()
  if not p.done:
    raise p.error(&"unexpected content after root {docType}")

# ---------------------------------------------------------------------------
# Writing / Serialization
# ---------------------------------------------------------------------------

proc escapeJsonString(s: string): string =
  ## Escape a string for JSON output (with quotes).
  result = "\""
  for c in s:
    case c
    of '"': result.add("\\\"")
    of '\\': result.add("\\\\")
    of '\b': result.add("\\b")
    of '\f': result.add("\\f")
    of '\n': result.add("\\n")
    of '\r': result.add("\\r")
    of '\t': result.add("\\t")
    else:
      if ord(c) < 0x20:
        result.add("\\u" & toHex(ord(c), 4).toLowerAscii)
      else:
        result.add(c)
  result.add('"')

proc writeNumber(output: var string, value: JsonNode) =
  if value.kind == JFloat:
    let f = value.getFloat
    if f.isNaN:
      output.add("nan")
    elif f == Inf:
      output.add("inf")
    elif f == NegInf:
      output.add("-inf")
    else:
      output.add($f)
  else:
    output.add($value.getBiggestInt)

proc writeString(output: var string, s: string, indent: int) =
  if '\n' in s:
    # Multiline string
    output.add("\"\"\"\n")
    let lines = s.split('\n')
    var processedLines = lines
    if processedLines.len > 0 and processedLines[^1] == "":
      processedLines.setLen(processedLines.len - 1)

    let contentIndent = " ".repeat(indent)
    for line in processedLines:
      output.add(contentIndent)
      output.add(line)
      output.add('\n')

    # Closing delimiter at key indent level
    output.add(" ".repeat(indent - MultilineIndent))
    output.add("\"\"\"")
  else:
    output.add(escapeJsonString(s))

proc writeDict(output: var string, d: JsonNode, indent: int) =
  if d.len == 0:
    output.add("{}")
    return

  var i = 0
  for key, value in d.pairs:
    if i > 0:
      output.add('\n')

    output.add(" ".repeat(indent))

    # Write key (quote if needed)
    let bareKeyChars = {'a'..'z', 'A'..'Z', '0'..'9', '-', '_'}
    var isBareKey = key.len > 0 and key[0].isAlphaAscii()
    if isBareKey:
      for c in key:
        if c notin bareKeyChars:
          isBareKey = false
          break

    if isBareKey:
      output.add(key)
    else:
      output.add(escapeJsonString(key))

    # Write value with appropriate indicator
    let isCollection = value.kind in {JObject, JArray}

    if isCollection:
      if value.len == 0:
        if value.kind == JArray:
          output.add(":: []")
        else:
          output.add(":: {}")
      else:
        output.add("::\n")
        output.writeValue(value, indent + MultilineIndent)
    else:
      output.add(": ")
      output.writeValue(value, indent + MultilineIndent)

    i += 1

proc writeList(output: var string, lst: JsonNode, indent: int) =
  if lst.len == 0:
    output.add("[]")
    return

  for i in 0 ..< lst.len:
    if i > 0:
      output.add('\n')

    output.add(" ".repeat(indent))
    output.add("- ")

    let value = lst[i]
    if value.kind in {JObject, JArray}:
      output.add("::\n")
      output.writeValue(value, indent + MultilineIndent)
    else:
      output.writeValue(value, indent)

proc writeValue(output: var string, value: JsonNode, indent: int) =
  case value.kind
  of JNull:
    output.add("null")
  of JBool:
    output.add(if value.getBool: "true" else: "false")
  of JInt, JFloat:
    output.writeNumber(value)
  of JString:
    output.writeString(value.getStr, indent)
  of JObject:
    output.writeDict(value, indent)
  of JArray:
    output.writeList(value, indent)

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

proc loads*(data: string): JsonNode =
  ## Parse HUML data and return a JsonNode.
  ##
  ## Raises HumlParseError if the input is not valid HUML.
  if data.len == 0:
    raise newException(HumlError, "empty document is undefined")
  var parser = newParser(data)
  parser.parseDocument()

proc dumps*(obj: JsonNode, indent: int = 0): string =
  ## Serialize a JsonNode to HUML format.
  ##
  ## Returns HUML formatted string.
  result = ""

  # Write version directive at document root
  if indent == 0:
    result.add(&"%HUML {SupportedVersion}\n")

  result.writeValue(obj, indent)

  if result.len > 0 and not result.endsWith('\n'):
    result.add('\n')

proc load*(filename: string): JsonNode =
  ## Load HUML from a file.
  loads(readFile(filename))

proc dump*(obj: JsonNode, filename: string) =
  ## Dump JsonNode as HUML to a file.
  writeFile(filename, dumps(obj))
