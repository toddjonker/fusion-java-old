// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonReader;
import com.amazon.ion.IonType;

/**
 * Utilities for working with {@link SyntaxValue}s.
 */
final class Syntax
{
    static SyntaxValue read(IonReader source)
    {
        IonType type = source.getType();
        assert type != null;

        switch (type)
        {
            case NULL:
            {
                return SyntaxNull.read(source);
            }
            case BOOL:
            {
                return SyntaxBool.read(source);
            }
            case INT:
            {
                return SyntaxInt.read(source);
            }
            case DECIMAL:
            {
                return SyntaxDecimal.read(source);
            }
            case FLOAT:
            {
                return SyntaxFloat.read(source);
            }
            case TIMESTAMP:
            {
                return SyntaxTimestamp.read(source);
            }
            case STRING:
            {
                return SyntaxString.read(source);
            }
            case SYMBOL:
            {
                return SyntaxSymbol.read(source);
            }
            case BLOB:
            {
                return SyntaxBlob.read(source);
            }
            case CLOB:
            {
                return SyntaxClob.read(source);
            }
            case LIST:
            {
                return SyntaxList.read(source);
            }
            case SEXP:
            {
                return SyntaxSexp.read(source);
            }
            case STRUCT:
            {
                return SyntaxStruct.read(source);
            }
        }

        throw new UnsupportedOperationException("Bad type: " + type);
    }
}
