import { parse } from './index';

const testScript = `' Example file.
function add
    parameter a   ' First argument.
    parameter b   ' Second argument.
    return a + b  ' Add them together.
end

print(add(1, 2))`;

const anyPos = () => ({
    "line": expect.any(Number),
    "col": expect.any(Number),
    "idx": expect.any(Number),
});

const resultTokens = [
    {
        "tag": "FUNCTION",
        "name": "add",
        "body": [
            {
                "tag": "PARAMETER",
                "pos": anyPos(),
            },
            {
                "tag": "WORD",
                "word": "a",
                "pos": anyPos(),
            },
            {
                "tag": "LINE_BREAK",
                "pos": anyPos(),
            },
            {
                "tag": "PARAMETER",
                "pos": anyPos(),
            },
            {
                "tag": "WORD",
                "word": "b",
                "pos": anyPos(),
            },
            {
                "tag": "LINE_BREAK",
                "pos": anyPos(),
            },
            {
                "tag": "RETURN",
                "pos": anyPos(),
            },
            {
                "tag": "WORD",
                "word": "a",
                "pos": anyPos(),
            },
            {
                "tag": "OP",
                "op": "ADD",
                "pos": anyPos(),
            },
            {
                "tag": "WORD",
                "word": "b",
                "pos": anyPos(),
            },
        ],
        "pos": anyPos(),
    },
    {
        "tag": "LINE_BREAK",
        "pos": anyPos(),
    },
    {
        "tag": "WORD",
        "word": "print",
        "pos": anyPos(),
    },
    {
        "tag": "B_SEQUENCE",
        "sequence": [
            {
                "tag": "WORD",
                "word": "add",
                "pos": anyPos(),
            },
            {
                "tag": "B_SEQUENCE",
                "sequence": [
                    {
                        "tag": "WORD",
                        "word": "1",
                        "pos": anyPos(),
                    },
                    {
                        "tag": "OP",
                        "op": "DELIM",
                        "pos": anyPos(),
                    },
                    {
                        "tag": "WORD",
                        "word": "2",
                        "pos": anyPos(),
                    }
                ],
                "bracket": "()",
                "pos": anyPos(),
            }
        ],
        "bracket": "()",
        "pos": anyPos(),
    }
];

describe('Parser', () => {
    it('should parse test-script to expected result', () => {
        expect(parse(testScript)).toEqual(resultTokens);
    });
});
