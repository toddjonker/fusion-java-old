// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonReader;
import com.amazon.ion.IonType;
import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxBool
    extends SyntaxValue
{
    private final Boolean myValue;


    private SyntaxBool(Boolean value)
    {
        myValue = value;
    }

    static SyntaxBool read(IonReader source)
    {
        Boolean value =
            (source.isNullValue() ? null : source.booleanValue());
        return new SyntaxBool(value);
    }


    @Override
    Type getType()
    {
        return Type.BOOL;
    }

    @Override
    FusionValue eval(Evaluator eval, Environment env) throws FusionException
    {
        return eval.newBool(myValue);
    }

    @Override
    FusionValue quote(Evaluator eval)
    {
        return eval.newBool(myValue);
    }

    @Override
    void writeTo(IonWriter writer) throws IOException
    {
        if (myValue == null)
        {
            writer.writeNull(IonType.BOOL);
        }
        else
        {
            writer.writeBool(myValue);
        }
    }

    @Override
    boolean isNullValue()
    {
        return myValue == null;
    }
}
