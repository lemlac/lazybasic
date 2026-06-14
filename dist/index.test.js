"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const index_1 = require("./index");
const testScript = `' Example file.
function add
    parameter a   ' First argument.
    parameter b   ' Second argument.
    return a + b  ' Add them together.
end

print(add(1, 2))`;
const resultTokens = [
    {
        "tag": "FUNCTION",
        "name": "add",
        "body": [
            {
                "pos": {
                    "line": 2,
                    "col": 13,
                    "idx": 42
                },
                "tag": "PARAMETER"
            },
            {
                "pos": {
                    "line": 2,
                    "col": 15,
                    "idx": 44
                },
                "tag": "WORD",
                "word": "a"
            },
            {
                "tag": "LINE_BREAK",
                "pos": {
                    "line": 2,
                    "col": 19,
                    "idx": 64
                }
            },
            {
                "pos": {
                    "line": 3,
                    "col": 13,
                    "idx": 78
                },
                "tag": "PARAMETER"
            },
            {
                "pos": {
                    "line": 3,
                    "col": 15,
                    "idx": 80
                },
                "tag": "WORD",
                "word": "b"
            },
            {
                "tag": "LINE_BREAK",
                "pos": {
                    "line": 3,
                    "col": 19,
                    "idx": 101
                }
            },
            {
                "pos": {
                    "line": 4,
                    "col": 10,
                    "idx": 112
                },
                "tag": "RETURN"
            },
            {
                "pos": {
                    "line": 4,
                    "col": 12,
                    "idx": 114
                },
                "tag": "WORD",
                "word": "a"
            },
            {
                "tag": "OP",
                "op": "ADD",
                "pos": {
                    "line": 4,
                    "col": 14,
                    "idx": 116
                }
            },
            {
                "pos": {
                    "line": 4,
                    "col": 16,
                    "idx": 118
                },
                "tag": "WORD",
                "word": "b"
            },
        ],
        "pos": {
            "line": 1,
            "col": 8,
            "idx": 24
        }
    },
    {
        "tag": "LINE_BREAK",
        "pos": {
            "line": 5,
            "col": 3,
            "idx": 144
        }
    },
    {
        "pos": {
            "line": 7,
            "col": 5,
            "idx": 151
        },
        "tag": "WORD",
        "word": "print"
    },
    {
        "tag": "B_SEQUENCE",
        "sequence": [
            {
                "pos": {
                    "line": 7,
                    "col": 9,
                    "idx": 155
                },
                "tag": "WORD",
                "word": "add"
            },
            {
                "tag": "B_SEQUENCE",
                "sequence": [
                    {
                        "pos": {
                            "line": 7,
                            "col": 11,
                            "idx": 157
                        },
                        "tag": "WORD",
                        "word": "1"
                    },
                    {
                        "tag": "OP",
                        "op": "DELIM",
                        "pos": {
                            "line": 7,
                            "col": 12,
                            "idx": 158
                        }
                    },
                    {
                        "pos": {
                            "line": 7,
                            "col": 14,
                            "idx": 160
                        },
                        "tag": "WORD",
                        "word": "2"
                    }
                ],
                "bracket": "()",
                "pos": {
                    "line": 7,
                    "col": 9,
                    "idx": 155
                }
            }
        ],
        "bracket": "()",
        "pos": {
            "line": 7,
            "col": 5,
            "idx": 151
        }
    }
];
describe('Parser', () => {
    it('should parse test-script to expected result', () => {
        expect((0, index_1.parse)(testScript)).toStrictEqual(resultTokens);
    });
});
//# sourceMappingURL=index.test.js.map