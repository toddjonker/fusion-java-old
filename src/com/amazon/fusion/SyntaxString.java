// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonReader;
import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxString
    extends SyntaxText
{
    private SyntaxString(String text)
    {
        super(text);
    }

    static SyntaxString read(IonReader source)
    {
        String text = source.stringValue();
        return new SyntaxString(text);
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
