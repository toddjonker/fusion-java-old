// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.ion.util.IonTextUtils.printString;
import org.junit.Before;
import org.junit.Test;

/**
 *
 */
public class IoTest
    extends CoreTestCase
{
    @Before
    public void requires()
        throws Exception
    {
        topLevel().requireModule("/fusion/eval");
        topLevel().requireModule("/fusion/io");
        topLevel().requireModule("/fusion/parameter");
    }


    @Test(expected = ArityFailure.class)
    public void testReadMoreArgs()
        throws Exception
    {
        eval("(read \"hi\")");
    }


    @Test
    public void testCurrentDirectory()
        throws Exception
    {
        String userDir = System.getProperty("user.dir");
        assertEval(printString(userDir), "(current_directory)");

        String newDir = userDir + "/tst-data";
        assertEval("\"hello\"",
                   "(parameterize" +
                   "  ((current_directory " + printString(newDir) + "))" +
                   "  (load \"hello.ion\"))");
    }

    @Test
    public void testLoadCurrentNamespace()
        throws Exception
    {
        eval("(load \"tst-data/trivialDefine.fusion\")");
        assertEval(3328, "x");
    }

    @Test
    public void testBadLoadCalls()
        throws Exception
    {
        expectArityFailure("(load)");
        expectArityFailure("(load \"x\" \"y\")");

        expectContractFailure("(load 12)");
    }
}
