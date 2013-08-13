// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


final class FusionString
{
    private FusionString() {}


    static final class AppendProc
        extends Procedure
    {
        AppendProc()
        {
            //    "                                                                               |
            super("Concatenates the `text` values (strings or symbols), returning a string.  If no\n" +
                  "arguments are supplied, the result is `\"\"`.",
                  "text", DOTDOTDOT);
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            StringBuilder resultBuilder = new StringBuilder();

            for (int i = 0; i < args.length; i++)
            {
                String v = checkTextArg(i, args);
                resultBuilder.append(v);
            }

            return eval.newString(resultBuilder.toString());
        }
    }



    static final class ToLowerProc
        extends Procedure
    {
        ToLowerProc()
        {
            super("Converts all the characters in a `string` to lower-case letters.",
                  "string");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(args);

            String input = checkStringArg(0, args);
            return eval.newString(input.toLowerCase());
        }
    }



    static final class ToUpperProc
        extends Procedure
    {
        ToUpperProc()
        {
            super("Converts all the characters in a `string` to upper-case letters.",
                  "string");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(args);

            String input = checkStringArg(0, args);
            return eval.newString(input.toUpperCase());
        }
    }



    static final class ToSymbolProc
        extends Procedure
    {
        ToSymbolProc()
        {
            //    "                                                                               |
            super("Converts a `string` to a symbol with the same text.  Returns `null.symbol` when\n"
                + "given `null.string`.  Raises an exception when given an empty string.",
                  "string");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(args);

            String input = checkNullableStringArg(0, args);

            if (input != null && input.isEmpty())
            {
                throw argFailure("non-empty string", 0, args);
            }

            return eval.newSymbol(input);
        }
    }
}
