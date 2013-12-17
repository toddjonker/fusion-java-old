// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionSymbol.makeSymbol;
import com.amazon.fusion.FusionSymbol.BaseSymbol;
import com.amazon.ion.IonException;
import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxKeyword
    extends SyntaxText
{
    /**
     * @param datum must not be null.
     */
    private SyntaxKeyword(SourceLocation loc, BaseSymbol datum)
    {
        super(loc, datum);
    }


    /**
     * @param datum must be a Fusion symbol.
     */
    static SyntaxKeyword make(Evaluator eval, SourceLocation loc, Object datum)
    {
        BaseSymbol symbol = (BaseSymbol) datum;
        return new SyntaxKeyword(loc, symbol);
    }


    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     */
    static SyntaxKeyword make(Evaluator eval,
                              SourceLocation loc,
                              String[] annotations,
                              String value)
    {
        BaseSymbol datum = makeSymbol(eval, annotations, value);
        return new SyntaxKeyword(loc, datum);
    }


    //========================================================================


    @Override
    SyntaxValue doExpand(Expander eval, Environment env)
        throws SyntaxException
    {
        throw new SyntaxException(null, "Keywords are not expressions", this);
    }


    @Override
    Object unwrap(Evaluator eval)
        throws FusionException
    {
        // TODO I have no idea if this is correct long-term.
        // Should we allow it at the moment?
        return super.unwrap(eval);
    }


    @Override
    void ionize(Evaluator eval, IonWriter writer)
        throws IOException, IonException, FusionException, IonizeFailure
    {
        // TODO __ ??
        super.ionize(eval, writer);
    }


    //========================================================================


    @Override
    CompiledForm doCompile(Evaluator eval, Environment env)
        throws FusionException
    {
        throw new IllegalStateException("Should not get here");
    }
}
