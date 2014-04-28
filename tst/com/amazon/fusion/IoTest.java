// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionIo.isEof;
import static com.amazon.fusion.FusionIo.read;
import static com.amazon.fusion.FusionStruct.isImmutableStruct;
import static com.amazon.fusion.FusionStruct.unsafeStructSize;
import static com.amazon.ion.util.IonTextUtils.printString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import com.amazon.ion.IonList;
import com.amazon.ion.IonReader;
import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
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
        expectArityExn("(load)");
        expectArityExn("(load \"x\" \"y\")");

        expectContractExn("(load 12)");
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


    /**
     * This attempts to have interesting combinations of data to trigger
     * various injection cases.
     */
    private static final String INJECTION_SAMPLE =
        "[   [],    (),    {},  " +
        " a::[], b::(), c::{},  " +
        " d::[ 1,               " +
        "      { f:null },      " +
        "      e::{ g:(sym) },  " +
        "      ('''str'''),     " +
        "    ],                 " +
        "]";


    private void writeInjectedDom(String ion)
        throws Exception
    {
        TopLevel  top  = topLevel();

        IonValue iv = system().singleValue(INJECTION_SAMPLE);
        Object fv = top.call("identity", iv);  // inject the value

        StringBuilder sb = new StringBuilder();

        FusionIo.write(top, fv, sb);

        assertEquals(iv, system().singleValue(sb.toString()));
    }

    @Test
    public void testWriteInjectedDom()
        throws Exception
    {
        writeInjectedDom(INJECTION_SAMPLE);
        writeInjectedDom("a::" + INJECTION_SAMPLE);
    }


    private void ionizeInjectedDom(String ion)
        throws Exception
    {
        TopLevel  top  = topLevel();

        IonValue iv = system().singleValue(ion);
        Object fv = top.call("identity", iv);  // inject the value

        IonList container = system().newEmptyList();
        IonWriter iw = system().newWriter(container);

        FusionIo.ionize(top, fv, iw);

        assertEquals(iv, container.get(0));
    }

    @Test
    public void testIonizeInjectedDom()
        throws Exception
    {
        ionizeInjectedDom(INJECTION_SAMPLE);
        ionizeInjectedDom("a::" + INJECTION_SAMPLE);
    }
}
