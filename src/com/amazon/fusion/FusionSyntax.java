// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.makeBool;
import static com.amazon.fusion.SyntaxValue.STX_PROPERTY_ORIGIN;


/**
 * Utilities for working with syntax objects.
 */
final class FusionSyntax
{
    private FusionSyntax() {}


    /**
     * Determines whether a Fusion value is a syntax object.
     */
    static boolean isSyntax(Evaluator eval, Object value)
    {
        return (value instanceof SyntaxValue);
    }


    /**
     * Determines whether a Fusion value is an identifier; that is, a syntax
     * object wrapping a symbol.
     */
    static boolean isIdentifier(Evaluator eval, Object value)
    {
        return (value instanceof SyntaxSymbol);
    }


    static Object unsafeSyntaxUnwrap(Evaluator eval, Object stx)
        throws FusionException
    {
        return ((SyntaxValue) stx).unwrap(eval);
    }

    static SourceLocation unsafeSyntaxLocation(Evaluator eval, Object stx)
    {
        return ((SyntaxValue) stx).getLocation();
    }


    /**
     * @param stx must be a syntax object.
     * @param key must not be null.
     * @return void if no value is associated with the key.
     */
    static Object unsafeSyntaxGetProperty(Evaluator eval,
                                          Object    stx,
                                          Object    key)
        throws FusionException
    {
        return ((SyntaxValue) stx).findProperty(eval, key);
    }


    /**
     * @param id1 must be an identifier.
     * @param id2 must be an identifier.
     */
    static boolean unsafeFreeIdentifierEqual(Evaluator eval,
                                             Object    id1,
                                             Object    id2)
        throws FusionException
    {
        return ((SyntaxSymbol) id1).freeIdentifierEqual((SyntaxSymbol) id2);
    }


    /**
     * @return void if the origin property isn't set.
     */
    static Object syntaxOrigin(Evaluator eval, SyntaxValue stx)
        throws FusionException
    {
        return stx.findProperty(eval, STX_PROPERTY_ORIGIN);
    }


    static SyntaxValue syntaxTrackOrigin(Evaluator eval,
                                         SyntaxValue newStx,
                                         SyntaxValue origStx,
                                         SyntaxSymbol origin)
        throws FusionException
    {
        return newStx.trackOrigin(eval, origStx, origin);
    }


    //========================================================================
    // Predicates

    static boolean unsafeSyntaxIsOriginal(Evaluator eval, Object stx)
        throws FusionException
    {
        return ((SyntaxValue) stx).isOriginal(eval);
    }


    //========================================================================
    // Procedure Helpers


    /**
     * @param expectation must not be null.
     */
    static SyntaxValue checkSyntaxArg(Evaluator eval,
                                      Procedure who,
                                      String    expectation,
                                      int       argNum,
                                      Object... args)
        throws ArgumentException
    {
        Object arg = args[argNum];
        if (arg instanceof SyntaxValue)
        {
            return (SyntaxValue) arg;
        }

        throw who.argFailure(expectation, argNum, args);
    }


    /**
     * @param expectation must not be null.
     */
    static SyntaxSymbol checkIdentifierArg(Evaluator eval,
                                           Procedure who,
                                           String    expectation,
                                           int       argNum,
                                           Object... args)
        throws ArgumentException
    {
        Object arg = args[argNum];
        if (arg instanceof SyntaxSymbol)
        {
            return (SyntaxSymbol) arg;
        }

        throw who.argFailure(expectation, argNum, args);
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
            boolean result = isIdentifier(eval, arg);
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
            boolean result = isSyntax(eval, arg);
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


    static final class PropertyProc
        extends Procedure
    {
        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityRange(2, 3, args);

            SyntaxValue stx =
                FusionSyntax.checkSyntaxArg(eval, this, "syntax object", 0, args);

            if (args.length == 2)
            {
                return stx.findProperty(eval, args[1]);
            }
            else
            {
                return stx.copyWithProperty(eval, args[1], args[2]);
            }
        }
    }


    static final class TrackOriginProc
        extends Procedure
    {
        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(3, args);

            SyntaxValue newStx =
                FusionSyntax.checkSyntaxArg(eval, this, "syntax object", 0, args);
            SyntaxValue origStx =
                FusionSyntax.checkSyntaxArg(eval, this, "syntax object", 1, args);
            SyntaxSymbol origin =
                checkIdentifierArg(eval, this, "syntax identifier", 2, args);

            return newStx.trackOrigin(eval, origStx, origin);
        }
    }


    static final class OriginProc
        extends Procedure
    {
        @Override
        Object doApply(Evaluator eval, Object[] args) throws FusionException
        {
            checkArityExact(1, args);

            SyntaxValue stx =
                FusionSyntax.checkSyntaxArg(eval, this, "syntax object", 0, args);

            return stx.findProperty(eval, STX_PROPERTY_ORIGIN);
        }
    }


    static final class IsOriginalProc
        extends Procedure
    {
        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(1, args);

            SyntaxValue stx =
                FusionSyntax.checkSyntaxArg(eval, this, "syntax object", 0, args);

            boolean o = stx.isOriginal(eval);
            return FusionBool.makeBool(eval, o);
        }
    }
}
