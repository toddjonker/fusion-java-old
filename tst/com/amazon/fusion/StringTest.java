// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Before;
import org.junit.Test;


public class StringTest
    extends CoreTestCase
{
    @Before
    public void requires()
        throws FusionException
    {
        topLevel().requireModule("/fusion/string");
    }


    @Test
    public void testStringAppendInvalid()
        throws Exception
    {
        expectArgumentExn("(string_append 1)",0);
        expectArgumentExn("(string_append true)",0);
    }


    @Test
    public void testStringCaseTransformFail()
        throws Exception
    {
        String [] ops = { "string_to_upper", "string_to_lower" };

        for (String op : ops)
        {
            expectArityExn("("+op+")");
            expectArityExn("("+op+" \"a\" \"b\")");

            expectContractExn("("+op+" {})");
            expectContractExn("("+op+" null.string)");
        }

    }

}
