// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.ion.util.IonTextUtils.printString;
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
                   "  (eval_file \"hello.ion\"))");

        assertEval("\"hello\"",
                   "(parameterize" +
                   "  ((current_directory " + printString(newDir) + "))" +
                   "  (load \"hello.ion\"))");
    }

    @Test
    public void testCurrentNamespace()
        throws Exception
    {
        eval("(eval_file \"tst-data/trivialDefine.ion\")");
        assertEval(3328, "x");
    }

    @Test
    public void testLoadCurrentNamespace()
        throws Exception
    {
        eval("(load \"tst-data/trivialDefine.ion\")");
        assertEval(3328, "x");
    }

    @Test
    public void testEvalFileSyntax()
        throws Exception
    {
        expectSyntaxFailure("(eval_file)");
        expectSyntaxFailure("(eval_file \"x\" \"y\")");

        expectContractFailure("(eval_file 12)");
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
