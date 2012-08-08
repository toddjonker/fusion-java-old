// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxString
    extends SyntaxText
{
    private SyntaxString(String value, String[] anns, SourceLocation loc)
    {
        super(value, anns, loc);
    }

    static SyntaxString make(String value, String[] anns, SourceLocation loc)
    {
        return new SyntaxString(value, anns, loc);
    }


    @Override
    Type getType()
    {
        return Type.STRING;
    }


    @Override
    FusionValue quote(Evaluator eval)
    {
        return eval.newString(myText, getAnnotations());
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
