// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.makeBool;


final class FusionSyntax
{
    private FusionSyntax() {}


    static SourceLocation unsafeSyntaxLocation(Evaluator eval, Object stx)
    {
        return ((SyntaxValue) stx).getLocation();
    }


    //========================================================================
    // Procedures

    static final class IsIdentifierProc
        extends Procedure1
    {
        IsIdentifierProc()
        {
            //    "                                                                               |
            super("Determines whether a `value` is a syntax object holding a symbol, returning\n" +
                  "`true` or `false`",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            boolean result = Syntax.isIdentifier(eval, arg);
            return makeBool(eval, result);
        }
    }


    static final class IsSyntaxProc
        extends Procedure1
    {
        IsSyntaxProc()
        {
            //    "                                                                               |
            super("Determines whether a `value` is a syntax object, returning `true` or `false`.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            boolean result = Syntax.isSyntax(eval, arg);
            return makeBool(eval, result);
        }
    }


    static final class ToDatumProc
        extends Procedure
    {
        ToDatumProc()
        {
            //    "                                                                               |
            super("Given a `syntax` object, removes the lexical information and returns a plain\n" +
                  "value, unwraps all layers recursively.",
                  "syntax");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(1, args);
            SyntaxValue stx = checkSyntaxArg(0, args);
            return stx.syntaxToDatum(eval);
        }
    }


    static final class UnwrapProc
        extends Procedure
    {
        UnwrapProc()
        {
            //    "                                                                               |
            super("Given a `syntax` object, removes the lexical information and returns a plain\n" +
                  "value. This only unwraps one layer, retaining inner syntax objects.",
                  "syntax");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(1, args);
            SyntaxValue stx = checkSyntaxArg(0, args);
            return stx.unwrap(eval);
        }
    }
}
