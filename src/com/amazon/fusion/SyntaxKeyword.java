// Copyright (c) 2012-2017 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.EMPTY_OBJECT_ARRAY;
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
    private SyntaxKeyword(SourceLocation loc,
                          Object[]       properties,
                          BaseSymbol     datum)
    {
        super(loc, properties, datum);
    }


    static SyntaxKeyword makeOriginal(Evaluator      eval,
                                      SourceLocation loc,
                                      BaseSymbol     symbol)
    {
        return new SyntaxKeyword(loc, ORIGINAL_STX_PROPS, symbol);
    }


    static SyntaxKeyword make(Evaluator      eval,
                              SourceLocation loc,
                              BaseSymbol     symbol)
    {
        return new SyntaxKeyword(loc, EMPTY_OBJECT_ARRAY, symbol);
    }


    //========================================================================


    @Override
    Object visit(Visitor v) throws FusionException
    {
        return v.accept(this);
    }


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
}
