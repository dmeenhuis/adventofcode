import fs from 'fs'

const wordDigits = [
  ['one', 1],
  ['two', 2],
  ['three', 3],
  ['four', 4],
  ['five', 5],
  ['six', 6],
  ['seven', 7],
  ['eight', 8],
  ['nine', 9],
]

const convertLineToDigits = (line, wordToDigitInputs) =>
  wordToDigitInputs.reduce((acc, [word, digit]) => acc.replace(word, `${digit}`), line)

const findFirstDigit = (charArray) => charArray.find((char) => !isNaN(parseInt(char)))

const calculateLineCalibrationValue = (line) =>
  parseInt(
    findFirstDigit([...convertLineToDigits(line, wordDigits)]) +
      findFirstDigit([...convertLineToDigits(line, wordDigits)].reverse())
  )

const calculateCalibrationValueForFile = (filename) =>
  fs
    .readFileSync(filename, 'utf-8')
    .split('\n')
    .reduce((acc, line) => acc + calculateLineCalibrationValue(line), 0)

console.log(calculateCalibrationValueForFile('day1-part1-test.txt'))
