// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Test;


public class DotTest
    extends CoreTestCase
{
    @Test
    public void testDotArity()
        throws Exception
    {
        expectArityExn("(.)");
    }

    @Test
    public void testNoParts()
        throws Exception
    {
        // FUnit still can't compare structs
        assertEval("{f:true}", "(. {f:true})");
    }


    @Test
    public void testNonContainerMidway()
        throws Exception
    {
        expectContractExn("(. [true] 0 1)");
        expectContractExn("(. [{f:null}] 0 \"f\" 0)");
    }
}
