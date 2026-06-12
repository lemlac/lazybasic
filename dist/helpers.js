"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.isWhitespace = isWhitespace;
exports.isAlphanumeric = isAlphanumeric;
exports.isSymbol = isSymbol;
exports.addQuotes = addQuotes;
function isWhitespace(s) {
    return /^\s+$/.test(s);
}
function isAlphanumeric(s) {
    return /^\w+$/.test(s);
}
function isSymbol(s) {
    return /^[~!@#$%^&*-+=\\|,<.>/?]+$/.test(s);
}
function addQuotes(s) {
    return s.replace(/\\/g, '\\\\').replace(/"/g, '\\"').replace(/^|$/g, '"');
}
//# sourceMappingURL=helpers.js.map