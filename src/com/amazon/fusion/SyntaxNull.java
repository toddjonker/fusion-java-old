// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxNull
    extends SyntaxValue
{
    private SyntaxNull(String[] anns, SourceLocation loc)
    {
        super(anns, loc);
    }

    static SyntaxNull make(String[] anns, SourceLocation loc)
    {
        return new SyntaxNull(anns, loc);
    }


    @Override
    Type getType()
    {
        return Type.NULL;
    }

    @Override
    Object doCompileIonConstant(Evaluator eval, Environment env)
        throws FusionException
    {
        return eval.newNull();
    }

    @Override
    Object unwrap(Evaluator eval, boolean recurse)
    {
        return eval.newNull(getAnnotations());
    }

    @Override
    void ionize(Evaluator eval, IonWriter writer) throws IOException
    {
        ionizeAnnotations(writer);
        writer.writeNull();
    }

    @Override
    boolean isNullValue()
    {
        return true;
    }
}
