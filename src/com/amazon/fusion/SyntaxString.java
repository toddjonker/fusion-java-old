// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxString
    extends SyntaxText
{
    private SyntaxString(String value, SourceLocation loc)
    {
        super(value, loc);
    }

    static SyntaxString make(String value, SourceLocation loc)
    {
        return new SyntaxString(value, loc);
    }


    @Override
    Type getType()
    {
        return Type.STRING;
    }


    @Override
    FusionValue quote(Evaluator eval)
    {
        return eval.newString(myText);
    }


    @Override
    FusionValue eval(Evaluator eval, Environment env)
        throws FusionException
    {
        return eval.newString(myText);
    }


    @Override
    void writeTo(IonWriter writer)
        throws IOException
    {
        writer.writeString(myText);
    }
}
