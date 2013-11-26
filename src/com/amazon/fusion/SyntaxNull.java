// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNull.makeNullNull;
import com.amazon.fusion.FusionNull.NullNull;
import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxNull
    extends SyntaxValue
{
    private SyntaxNull(SourceLocation loc, String[] anns)
    {
        super(anns, loc);
    }


    private SyntaxNull(SourceLocation loc, NullNull datum)
    {
        super(datum.annotationsAsJavaStrings(), loc);
    }


    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     */
    static SyntaxNull make(Evaluator eval,
                           SourceLocation loc,
                           String[] annotations)
    {
        return new SyntaxNull(loc, annotations);
    }


    /**
     * @param datum must be a Fusion null.null.
     */
    static SyntaxNull make(Evaluator eval, SourceLocation loc, Object datum)
    {
        NullNull n = (NullNull) datum;
        return new SyntaxNull(loc, n);
    }


    //========================================================================


    @Override
    Type getType()
    {
        return Type.NULL;
    }

    @Override
    CompiledForm doCompile(Evaluator eval, Environment env)
        throws FusionException
    {
        Object fv = makeNullNull(eval, getAnnotations());
        return new CompiledConstant(fv);
    }

    @Override
    Object unwrap(Evaluator eval, boolean recurse)
    {
        return makeNullNull(eval, getAnnotations());
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
