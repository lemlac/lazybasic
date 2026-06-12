"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.parse = exports.tokenizeFile = exports.tokenizeStream = exports.tokenize = void 0;
var tokenizer_1 = require("./tokenizer");
Object.defineProperty(exports, "tokenize", { enumerable: true, get: function () { return tokenizer_1.tokenize; } });
Object.defineProperty(exports, "tokenizeStream", { enumerable: true, get: function () { return tokenizer_1.tokenizeStream; } });
Object.defineProperty(exports, "tokenizeFile", { enumerable: true, get: function () { return tokenizer_1.tokenizeFile; } });
var parser_1 = require("./parser");
Object.defineProperty(exports, "parse", { enumerable: true, get: function () { return parser_1.parse; } });
//# sourceMappingURL=index.js.map