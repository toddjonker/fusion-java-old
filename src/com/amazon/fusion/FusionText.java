// Copyright (c) 2013-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.falseBool;
import static com.amazon.fusion.FusionBool.trueBool;
import com.amazon.fusion.FusionBool.BaseBool;


/**
 *
 */
final class FusionText
{
    private FusionText() {}


    abstract static class BaseText
        extends BaseValue
    {
        BaseText() {}

        abstract String stringValue();

        @Override
        BaseBool looseEquals(Evaluator eval, Object right)
            throws FusionException
        {
            if (right instanceof BaseText)
            {
                String r = ((BaseText) right).stringValue();
                if (r != null)
                {
                    String l = this.stringValue(); // not null
                    if (l.equals(r))
                    {
                        return trueBool(eval);
                    }
                }
            }

            return falseBool(eval);
        }
    }


    //========================================================================
    // Predicates


    public static boolean isText(TopLevel top, Object value)
        throws FusionException
    {
        return (value instanceof BaseText);
    }


    static boolean isText(Evaluator eval, Object value)
        throws FusionException
    {
        return (value instanceof BaseText);
    }


    //========================================================================
    // Conversions


    /**
     * @param stringOrSymbol must be a Fusion string or symbol.
     *
     * @return null if given {@code null.string} or {@code null.symbol}.
     */
    static String unsafeTextToJavaString(Evaluator eval, Object stringOrSymbol)
        throws FusionException
    {
        return ((BaseText) stringOrSymbol).stringValue();
    }


    /**
     * Converts a Fusion text value to a {@link String}.
     *
     * @return null if the value isn't a Fusion string or symbol.
     */
    static String textToJavaString(Evaluator eval, Object value)
        throws FusionException
    {
        if (isText(eval, value))
        {
            return unsafeTextToJavaString(eval, value);
        }
        return null;
    }


    //========================================================================
    // Procedure Helpers


    /**
     * @param expectation must not be null.
     * @return may be null
     */
    static String checkTextArg(Evaluator eval,
                               Procedure who,
                               String    expectation,
                               int       argNum,
                               Object... args)
        throws FusionException, ArgTypeFailure
    {
        Object arg = args[argNum];
        if (arg instanceof BaseText)
        {
            return ((BaseText) arg).stringValue();
        }
        throw who.argFailure(expectation, argNum, args);
    }


    /**
     * @return not null.
     */
    static String checkRequiredTextArg(Evaluator eval,
                                       Procedure who,
                                       int       argNum,
                                       Object... args)
        throws FusionException, ArgTypeFailure
    {
        String expectation = "non-null string or symbol";
        String result = checkTextArg(eval, who, expectation, argNum, args);
        if (result == null)
        {
            throw who.argFailure(expectation, argNum, args);
        }
        return result;
    }


    /**
     * @return not null or empty
     */
    static String checkNonEmptyTextArg(Evaluator eval,
                                       Procedure who,
                                       int       argNum,
                                       Object... args)
        throws FusionException, ArgTypeFailure
    {
        String expectation = "non-empty string or symbol";
        String result = checkTextArg(eval, who, expectation, argNum, args);
        if (result == null || result.isEmpty())
        {
            throw who.argFailure(expectation, argNum, args);
        }
        return result;
    }
}
