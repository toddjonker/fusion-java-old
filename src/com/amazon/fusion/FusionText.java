// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionString.isString;
import static com.amazon.fusion.FusionString.unsafeStringToJavaString;
import com.amazon.fusion.FusionString.BaseString;
import com.amazon.ion.IonText;
import com.amazon.ion.IonValue;

/**
 *
 */
final class FusionText
{
    private FusionText() {}


    //========================================================================
    // Predicates


    public static boolean isText(TopLevel top, Object value)
        throws FusionException
    {
        return (value instanceof BaseString || value instanceof IonText);
    }


    static boolean isText(Evaluator eval, Object value)
        throws FusionException
    {
        return (value instanceof BaseString || value instanceof IonText);
    }


    //========================================================================
    // Conversions


    /**
     * @return null if given {@code null.string} or {@code null.symbol}.
     */
    static String unsafeTextToJavaString(Evaluator eval, Object stringOrSymbol)
        throws FusionException
    {
        if (stringOrSymbol instanceof BaseString)
        {
            return unsafeStringToJavaString(eval, stringOrSymbol);
        }
        return ((IonText) stringOrSymbol).stringValue();
    }


    /**
     * Converts a Fusion text value to a {@link String}.
     *
     * @return null if the value isn't a Fusion string or symbol.
     */
    static String textToJavaString(Evaluator eval, Object value)
        throws FusionException
    {
        if (isString(eval, value))
        {
            return unsafeStringToJavaString(eval, value);
        }

        IonValue iv = FusionValue.castToIonValueMaybe(value);
        if (iv != null && iv instanceof IonText)
        {
            return ((IonText) iv).stringValue();
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
        if (arg instanceof BaseString)
        {
            return ((BaseString) arg).stringValue();
        }

        IonText iv = who.checkDomArg(IonText.class, expectation,
                                     true /* nullable */, argNum, args);
        return iv.stringValue();
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
