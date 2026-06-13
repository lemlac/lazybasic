"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.parseTokens = exports.tokenizeFile = exports.tokenizeStream = exports.tokenize = void 0;
exports.parse = parse;
exports.parseFile = parseFile;
exports.parseFileAndPrint = parseFileAndPrint;
const tokenizer_1 = require("./tokenizer");
Object.defineProperty(exports, "tokenize", { enumerable: true, get: function () { return tokenizer_1.tokenize; } });
Object.defineProperty(exports, "tokenizeStream", { enumerable: true, get: function () { return tokenizer_1.tokenizeStream; } });
Object.defineProperty(exports, "tokenizeFile", { enumerable: true, get: function () { return tokenizer_1.tokenizeFile; } });
const parser_1 = require("./parser");
Object.defineProperty(exports, "parseTokens", { enumerable: true, get: function () { return parser_1.parseTokens; } });
function parse(input) {
    return (0, parser_1.parseTokens)((0, tokenizer_1.tokenize)(input));
}
async function parseFile(filename) {
    return (0, parser_1.parseTokens)(await (0, tokenizer_1.tokenizeFile)(filename), filename);
}
async function parseFileAndPrint(filename) {
    console.log(JSON.stringify(await parseFile(filename), null, 2));
}
//# sourceMappingURL=index.js.map