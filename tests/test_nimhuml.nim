import std/[unittest, json, os, strutils]
import ../src/nimhuml

const TestDataDir = "tests/tests"

suite "HUML Assertion Tests":
  let assertionFile = TestDataDir / "assertions" / "mixed.json"
  if not fileExists(assertionFile):
    echo "WARNING: Test data not found at ", assertionFile
    echo "Run: cd tests && git clone https://github.com/huml-lang/tests.git"
  else:
    let testCases = parseFile(assertionFile)

    for tc in testCases:
      let name = tc["name"].getStr
      let input = tc["input"].getStr
      let errorExpected = tc["error"].getBool

      test name:
        if errorExpected:
          expect(HumlError, HumlParseError):
            discard loads(input)
        else:
          try:
            discard loads(input)
          except HumlError as e:
            echo "Unexpected error: ", e.msg
            check false

suite "HUML Document Tests":
  let docsDir = TestDataDir / "documents"
  if not dirExists(docsDir):
    echo "WARNING: Documents directory not found at ", docsDir
  else:
    for humlPath in walkFiles(docsDir / "*.huml"):
      let jsonPath = humlPath[0 ..< humlPath.len - 5] & ".json"
      if fileExists(jsonPath):
        let baseName = extractFilename(humlPath)

        test "parse " & baseName:
          let humlContent = readFile(humlPath)
          let resHuml = loads(humlContent)
          let resJson = parseFile(jsonPath)
          check resHuml == resJson

suite "HUML Encode Round-Trip Tests":
  let docsDir = TestDataDir / "documents"
  if not dirExists(docsDir):
    echo "WARNING: Documents directory not found at ", docsDir
  else:
    for humlPath in walkFiles(docsDir / "*.huml"):
      let jsonPath = humlPath[0 ..< humlPath.len - 5] & ".json"
      if fileExists(jsonPath):
        let baseName = extractFilename(humlPath)

        test "round-trip " & baseName:
          let humlContent = readFile(humlPath)
          let resHuml = loads(humlContent)
          let marshalled = dumps(resHuml)
          let resConverted = loads(marshalled)
          let resJson = parseFile(jsonPath)
          check resConverted == resJson
