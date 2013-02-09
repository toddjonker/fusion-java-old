// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


final class FusionSyntax
{
    private FusionSyntax() {}


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
            return eval.newBool(result);
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
            return eval.newBool(result);
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
            return stx.unwrap(eval, true);
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
            return stx.unwrap(eval, false);
        }
    }
}
