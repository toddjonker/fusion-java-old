// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.SourceLocation.currentLocation;
import com.amazon.ion.Decimal;
import com.amazon.ion.IonReader;
import com.amazon.ion.IonType;
import com.amazon.ion.Timestamp;

/**
 * Utilities for working with {@link SyntaxValue}s.
 */
final class Syntax
{
    static SyntaxValue read(IonReader source)
    {
        IonType type = source.getType();
        assert type != null;

        SourceLocation loc = currentLocation(source);

        switch (type)
        {
            case NULL:
            {
                return SyntaxNull.make(loc);
            }
            case BOOL:
            {
                Boolean value =
                    (source.isNullValue() ? null : source.booleanValue());
                return SyntaxBool.make(value, loc);
            }
            case INT:
            {
                return SyntaxInt.make(source.bigIntegerValue(), loc);
            }
            case DECIMAL:
            {
                // TODO WORKAROUND ION-290 this check shouldn't be needed
                Decimal value =
                    (source.isNullValue() ? null : source.decimalValue());
                return SyntaxDecimal.make(value, loc);
            }
            case FLOAT:
            {
                Double value =
                    (source.isNullValue() ? null : source.doubleValue());
                return SyntaxFloat.make(value, loc);
            }
            case TIMESTAMP:
            {
                Timestamp value = source.timestampValue();
                return SyntaxTimestamp.make(value, loc);
            }
            case STRING:
            {
                String value = source.stringValue();
                return SyntaxString.make(value, loc);
            }
            case SYMBOL:
            {
                String value = source.stringValue();
                return SyntaxSymbol.make(value, loc);
            }
            case BLOB:
            {
                byte[] value =
                    (source.isNullValue() ? null : source.newBytes());
                return SyntaxBlob.make(value, loc);
            }
            case CLOB:
            {
                byte[] value =
                    (source.isNullValue() ? null : source.newBytes());
                return SyntaxClob.make(value, loc);
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
