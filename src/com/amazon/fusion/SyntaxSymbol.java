// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonReader;
import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxSymbol
    extends SyntaxText
{
    static SyntaxSymbol make(String text)
    {
        return new SyntaxSymbol(text);
    }


    private SyntaxSymbol(String text)
    {
        super(text);
    }

    static SyntaxSymbol read(IonReader source)
    {
        String text = source.stringValue();
        return new SyntaxSymbol(text);
    }


    @Override
    Type getType()
    {
        return Type.SYMBOL;
    }


    @Override
    FusionValue quote(Evaluator eval)
    {
        return eval.newSymbol(myText);
    }


    @Override
    FusionValue eval(Evaluator eval, Environment env)
        throws FusionException
    {
        if (myText == null)
        {
            throw new SyntaxFailure(null, "null.symbol is not an expression",
                                    this);
        }

        FusionValue result = env.lookup(myText);
        if (result == null)
        {
            throw new UnboundIdentifierFailure(null, this);
        }
        return result;
    }


    @Override
    void writeTo(IonWriter writer)
        throws IOException
    {
        writer.writeSymbol(myText);
    }
}
