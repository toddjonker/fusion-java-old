// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
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

    static SyntaxString make(String value)
    {
        return new SyntaxString(value, EMPTY_STRING_ARRAY, null);
    }


    @Override
    Type getType()
    {
        return Type.STRING;
    }


    @Override
    Object quote(Evaluator eval)
    {
        return eval.newString(myText, getAnnotations());
    }


    @Override
    Object doCompileIonConstant(Evaluator eval, Environment env)
        throws FusionException
    {
        return eval.newString(myText);
    }


    @Override
    void writeContentTo(IonWriter writer)
        throws IOException
    {
        writer.writeString(myText);
    }
}
