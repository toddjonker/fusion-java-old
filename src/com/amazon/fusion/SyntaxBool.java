// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.makeBool;
import com.amazon.fusion.FusionBool.BaseBool;
import com.amazon.ion.IonException;
import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxBool
    extends SyntaxValue
{
    private final BaseBool myDatum;


    private SyntaxBool(SourceLocation loc, BaseBool datum)
    {
        super(datum.annotationsAsJavaStrings(), loc);
        myDatum = datum;
    }


    /**
     * @param datum must be a Fusion bool.
     */
    static SyntaxBool make(Evaluator eval, SourceLocation loc, Object datum)
    {
        BaseBool bool = (BaseBool) datum;
        return new SyntaxBool(loc, bool);
    }


    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     * @param value may be null.
     */
    static SyntaxBool make(Evaluator eval,
                           SourceLocation loc,
                           String[] annotations,
                           Boolean value)
    {
        BaseBool b = makeBool(eval, annotations, value);
        return new SyntaxBool(loc, b);
    }


    //========================================================================


    @Override
    Type getType()
    {
        return Type.BOOL;
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
