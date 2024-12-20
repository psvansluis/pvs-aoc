import fs from "node:fs";

// const data = fs.readFileSync("testData.txt", "utf8");
const data = fs.readFileSync("realData.txt", "utf8");
const lines = data.split("\n").map((line) => line.split("   "));
const obj = lines.reduce(
  ({ left, right }, curr) => ({
    left: [...left, curr[0]],
    right: [...right, curr[1]],
  }),
  { left: [], right: [] }
);
const sorted = {
  left: obj.left.sort(),
  right: obj.right.sort(),
};
const diffs = sorted.left.map((left, i) => Math.abs(left - sorted.right[i]));
const sum = diffs.reduce((a, b) => a + b, 0);
console.log("sum " + sum);

// part 2
const occurencesInRight = obj.right.reduce(
  (obj, item) => ({ ...obj, [item]: (obj[item] ?? 0) + 1 }),
  {}
);
const similarityScores = obj.left.map(
  (val) => val * (occurencesInRight[val] ?? 0)
);
const sumSimilarityScores = similarityScores.reduce((a, b) => a + b, 0);
console.log("sum of similarityScores " + sumSimilarityScores);
