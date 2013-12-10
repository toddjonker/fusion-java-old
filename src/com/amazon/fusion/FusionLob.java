// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 *
 */
final class FusionLob
{
    private FusionLob() {}


    abstract static class BaseLob
        extends BaseValue
    {
        BaseLob() {}
    }


    //========================================================================
    // Predicates


    public static boolean isLob(TopLevel top, Object value)
        throws FusionException
    {
        return (value instanceof BaseLob);
    }


    static boolean isLob(Evaluator eval, Object value)
        throws FusionException
    {
        return (value instanceof BaseLob);
    }


    //========================================================================
    // Conversions


    //========================================================================
    // Procedure Helpers


    // TODO is_lob
}
