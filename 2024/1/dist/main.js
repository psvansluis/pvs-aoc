"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const node_fs_1 = __importDefault(require("node:fs"));
// const data = fs.readFileSync("testData.txt", "utf8");
const data = node_fs_1.default.readFileSync("realData.txt", "utf8");
const lines = data.split("\n").map((line) => line.split("   "));
const obj = lines.reduce(({ left, right }, curr) => ({
    left: [...left, curr[0]],
    right: [...right, curr[1]],
}), { left: [], right: [] });
const sorted = {
    left: obj.left.sort(),
    right: obj.right.sort(),
};
const diffs = sorted.left.map((left, i) => Math.abs(left - sorted.right[i]));
const sum = diffs.reduce((a, b) => a + b, 0);
console.log(sum);
//# sourceMappingURL=main.js.map