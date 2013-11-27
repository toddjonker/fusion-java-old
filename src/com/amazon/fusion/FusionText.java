// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonText;

/**
 *
 */
final class FusionText
{
    private FusionText() {}


    /**
     * @return null if given {@code null.string} or {@code null.symbol}.
     */
    static String unsafeTextToString(Evaluator eval, Object stringOrSymbol)
        throws FusionException
    {
        return ((IonText) stringOrSymbol).stringValue();
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
        throws ArgTypeFailure
    {
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
        throws ArgTypeFailure
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
        throws ArgTypeFailure
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
