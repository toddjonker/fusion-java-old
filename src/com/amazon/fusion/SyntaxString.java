// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

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
    Object unwrap(Evaluator eval, boolean recurse)
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
    void ionize(Evaluator eval, IonWriter writer)
        throws IOException
    {
        ionizeAnnotations(writer);
        writer.writeString(myText);
    }
}
