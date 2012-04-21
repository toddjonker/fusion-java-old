// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Test;

/**
 *
 */
public class IoTest
    extends CoreTestCase
{
    @Test(expected = ArityFailure.class)
    public void testReadMoreArgs()
        throws Exception
    {
        eval("(read \"hi\")");
    }
}
