// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonException;
import com.amazon.ion.IonWriter;
import java.io.IOException;

/**
 *
 */
abstract class SimpleSyntaxValue
    extends SyntaxValue
{
    final BaseValue myDatum;


    /**
     * @param datum must not be null.
     */
    SimpleSyntaxValue(SourceLocation loc, BaseValue datum)
    {
        super(loc);
        myDatum = datum;
    }


    @Override
    String[] annotationsAsJavaStrings()
    {
        return myDatum.annotationsAsJavaStrings();
    }

    @Override
    abstract Type getType();


    @Override
    CompiledForm doCompile(Evaluator eval, Environment env)
        throws FusionException
    {
        return new CompiledConstant(myDatum);
    }

    @Override
    Object unwrap(Evaluator eval)
        throws FusionException
    {
        return myDatum;
    }

    @Override
    Object syntaxToDatum(Evaluator eval)
        throws FusionException
    {
        return myDatum;
    }

    @Override
    boolean isNullValue()
    {
        return myDatum.isAnyNull();
    }

    @Override
    void ionize(Evaluator eval, IonWriter writer)
        throws IOException, IonException, FusionException, IonizeFailure
    {
        myDatum.ionize(eval, writer);
    }
}
