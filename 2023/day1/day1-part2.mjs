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

const convertLineToDigits = (line, wordToDigitInputs, direction) =>
  wordToDigitInputs
    .reduce((acc, [word, digit]) => {
      const matches = [...line.matchAll(word)]
      const found = matches.reduce((matchAcc, match) => [...matchAcc, { word, digit, index: match.index }], [])

      return [...acc, ...found]
    }, [])
    .sort((a, b) => (direction === 'ltr' ? a.index - b.index : b.index - a.index))
    .reduce(
      (acc, { word, digit }) =>
        acc.replace(direction === 'rtl' ? new RegExp(`(${word})(?!.*\\1)`) : word, digit.toString()),
      line
    )

const findFirstDigit = (charArray) => charArray.find((char) => !isNaN(parseInt(char)))

const calculateLineCalibrationValue = (line) =>
  parseInt(
    findFirstDigit([...convertLineToDigits(line, wordDigits, 'ltr')]) +
      findFirstDigit([...convertLineToDigits(line, wordDigits, 'rtl')].reverse())
  )

const calculateCalibrationValueForFile = (filename) =>
  fs
    .readFileSync(filename, 'utf-8')
    .split('\n')
    .reduce((acc, line) => {
      console.log(`${line},${calculateLineCalibrationValue(line)}`)
      return acc + calculateLineCalibrationValue(line)
    }, 0)

console.log(calculateCalibrationValueForFile('day1.txt'))
