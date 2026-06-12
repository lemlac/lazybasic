import type { TokenWithTracking } from './types';

import { tokenize, tokenizeStream, tokenizeFile } from './tokenizer';
import { parse } from './parser';

export { tokenize, tokenizeStream, tokenizeFile, parse };

export async function parseFile(filename: string): Promise<TokenWithTracking[]> {
    return parse(await tokenizeFile(filename), filename);
}

export async function parseFileAndPrint(filename: string): Promise<void> {
    console.log(JSON.stringify(await parseFile(filename), null, 2));
}
