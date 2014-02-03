// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionIo.isEof;
import static com.amazon.fusion.FusionIo.read;
import static com.amazon.fusion.FusionStruct.isImmutableStruct;
import static com.amazon.fusion.FusionStruct.unsafeStructSize;
import static com.amazon.ion.util.IonTextUtils.printString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import com.amazon.ion.IonReader;
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


    @Test
    public void testFfiRead()
        throws Exception
    {
        TopLevel  top  = topLevel();
        Evaluator eval = evaluator();

        IonReader reader = system().newReader("{}");
        Object fv = read(top, reader);
        assertTrue(isImmutableStruct(eval, fv));
        assertEquals(0, unsafeStructSize(eval, fv));
        fv = read(top, reader);
        assertTrue(isEof(top, fv));

        reader = system().newReader("{f:9} 10");
        reader.next();
        fv = FusionIo.read(top, reader);
        assertTrue(isImmutableStruct(eval, fv));
        assertEquals(1, unsafeStructSize(eval, fv));
        fv = read(top, reader);
        checkLong(10, fv);
        fv = read(top, reader);
        assertTrue(isEof(top, fv));
        fv = read(top, reader);
        assertTrue(isEof(top, fv));  // EOF "sticks"
    }
}
