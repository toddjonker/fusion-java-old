// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBlob.makeBlob;
import com.amazon.fusion.FusionBlob.BaseBlob;
import com.amazon.ion.IonException;
import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxBlob
    extends SyntaxValue
{
    private final BaseBlob myDatum;


    private SyntaxBlob(SourceLocation loc, BaseBlob datum)
    {
        super(datum.annotationsAsJavaStrings(), loc);
        myDatum = datum;
    }


    /**
     * @param datum must be a Fusion blob.
     */
    static SyntaxBlob make(Evaluator eval, SourceLocation loc, Object datum)
    {
        return new SyntaxBlob(loc, (BaseBlob) datum);
    }


    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     * @param value may be null.
     * This method assumes ownership of the array and it must not be modified
     * later.
     */
    static SyntaxBlob make(Evaluator      eval,
                           SourceLocation loc,
                           String[]       annotations,
                           byte[]         value)
    {
        BaseBlob datum = makeBlob(eval, annotations, value);
        return new SyntaxBlob(loc, datum);
    }


    //========================================================================


    @Override
    Type getType()
    {
        return Type.BLOB;
    }

    @Override
    CompiledForm doCompile(Evaluator eval, Environment env)
        throws FusionException
    {
        return new CompiledConstant(myDatum);
    }

    @Override
    Object unwrap(Evaluator eval, boolean recurse)
    {
        return myDatum;
    }

    @Override
    void ionize(Evaluator eval, IonWriter writer)
        throws IOException, IonException, FusionException, IonizeFailure
    {
        myDatum.ionize(eval, writer);
    }

    @Override
    boolean isNullValue()
    {
        return myDatum.isAnyNull();
    }
}
