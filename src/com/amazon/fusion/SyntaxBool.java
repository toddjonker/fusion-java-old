// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonType;
import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxBool
    extends SyntaxValue
{
    private final Boolean myValue;


    private SyntaxBool(Boolean value, String[] anns, SourceLocation loc)
    {
        super(anns, loc);
        myValue = value;
    }

    static SyntaxBool make(Boolean value, String[] anns, SourceLocation loc)
    {
        return new SyntaxBool(value, anns, loc);
    }


    @Override
    Type getType()
    {
        return Type.BOOL;
    }

    @Override
    Object doCompileIonConstant(Evaluator eval, Environment env)
        throws FusionException
    {
        return eval.newBool(myValue);
    }

    @Override
    Object quote(Evaluator eval)
    {
        return eval.newBool(myValue, getAnnotations());
    }

    @Override
    void writeContentTo(IonWriter writer) throws IOException
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
