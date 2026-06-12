export function isWhitespace(s: string): boolean {
    return /^\s+$/.test(s);
}

export function isAlphanumeric(s: string): boolean {
    return /^\w+$/.test(s);
}

export function isSymbol(s: string): boolean {
    return /^[~!@#$%^&*-+=\\|,<.>/?]+$/.test(s);
}

export function addQuotes(s: string): string {
    return s.replace(/\\/g, '\\\\').replace(/"/g, '\\"').replace(/^|$/g, '"');
}
