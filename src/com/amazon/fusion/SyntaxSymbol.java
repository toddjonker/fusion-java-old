// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxSymbol
    extends SyntaxText
{
    private SyntaxSymbol(String value, SourceLocation loc)
    {
        super(value, loc);
    }

    static SyntaxSymbol make(String value)
    {
        return new SyntaxSymbol(value, null);
    }

    static SyntaxSymbol make(String value, SourceLocation loc)
    {
        return new SyntaxSymbol(value, loc);
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
