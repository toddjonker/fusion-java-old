// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionText.checkRequiredTextArg;
import com.amazon.ion.IonString;
import com.amazon.ion.IonValue;


final class FusionString
{
    private FusionString() {}


    //========================================================================
    // Constructors


    static Object makeString(Evaluator eval, String value)
    {
        return eval.getSystem().newString(value);
    }


    static Object makeString(Evaluator eval,
                             String[]  annotations,
                             String    value)
    {
        IonValue dom = eval.getSystem().newString(value);
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return dom;
    }


    //========================================================================
    // Predicates


    public static boolean isString(TopLevel top, Object value)
        throws FusionException
    {
        return (value instanceof IonString);
    }

    static boolean isString(Evaluator eval, Object value)
        throws FusionException
    {
        return (value instanceof IonString);
    }


    //========================================================================
    // Conversions


    static String unsafeStringToJavaString(Evaluator eval, Object value)
        throws FusionException
    {
        return ((IonString) value).stringValue();
    }


    /**
     * Converts a Fusion string to a {@link String}.
     *
     * @return null if the value isn't a Fusion string.
     */
    static String stringToJavaString(Evaluator eval, Object value)
        throws FusionException
    {
        if (isString(eval, value))
        {
            return unsafeStringToJavaString(eval, value);
        }
        return null;
    }


    //========================================================================
    // Procedure Helpers


    /**
     * @param expectation must not be null.
     * @return may be null
     */
    static String checkStringArg(Evaluator eval,
                                 Procedure who,
                                 String    expectation,
                                 int       argNum,
                                 Object... args)
        throws ArgTypeFailure
    {
        IonString iv = who.checkDomArg(IonString.class, expectation,
                                       true /* nullable */, argNum, args);
        return iv.stringValue();
    }


    /**
     * @return may be null
     */
    static String checkNullableStringArg(Evaluator eval,
                                         Procedure who,
                                         int       argNum,
                                         Object... args)
        throws FusionException, ArgTypeFailure
    {
        String expectation = "nullable string";
        return checkStringArg(eval, who, expectation, argNum, args);
    }


    /**
     * @return not null
     */
    static String checkRequiredStringArg(Evaluator eval,
                                         Procedure who,
                                         int       argNum,
                                         Object... args)
        throws FusionException, ArgTypeFailure
    {
        String expectation = "non-null string";
        String result = checkStringArg(eval, who, expectation, argNum, args);
        if (result == null)
        {
            throw who.argFailure(expectation, argNum, args);
        }
        return result;
    }


    /**
     * @return not null or empty
     */
    static String checkNonEmptyStringArg(Evaluator eval,
                                         Procedure who,
                                         int       argNum,
                                         Object... args)
        throws FusionException, ArgTypeFailure
    {
        String expectation = "non-empty string";
        String result = checkStringArg(eval, who, expectation, argNum, args);
        if (result == null || result.isEmpty())
        {
            throw who.argFailure(expectation, argNum, args);
        }
        return result;
    }


    //========================================================================
    // Procedures


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
                String v = checkRequiredTextArg(eval, this, i, args);
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

            String input = checkRequiredStringArg(eval, this, 0, args);
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

            String input = checkRequiredStringArg(eval, this, 0, args);
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

            String input = checkNullableStringArg(eval, this, 0, args);

            if (input != null && input.isEmpty())
            {
                throw argFailure("non-empty string", 0, args);
            }

            return eval.newSymbol(input);
        }
    }
}
