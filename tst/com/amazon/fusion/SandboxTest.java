// Copyright (c) 2022 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.ion.util.IonTextUtils.printString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import java.io.File;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

public class SandboxTest
    extends CoreTestCase
{
    public static final File HELLO_RELATIVE = new File("tst-data/hello.ion");
    public static final File HELLO_ABSOLUTE = HELLO_RELATIVE.getAbsoluteFile();


    @Rule
    public ExpectedException thrown = ExpectedException.none();

    private void expectAccessDenied(String who, File what)
    {
        // TODO: be more specific
        thrown.expect(FusionErrorException.class);
        thrown.expectMessage(who + ": Access denied to " + what);
    }


    private TopLevel sandbox()
        throws FusionException
    {
        SandboxBuilder b = runtime().makeSandboxBuilder();
        b.setLanguage("/fusion");
        return b.build();
    }


    @Test
    public void testNoLanguageProvided()
        throws Exception
    {
        SandboxBuilder b = runtime().makeSandboxBuilder();

        thrown.expect(IllegalStateException.class);
        b.build();
    }


    @Test
    public void testSandboxCreation()
        throws Exception
    {
        Object result = sandbox().eval("123");
        checkLong(123, result);
    }

    private void withIonFromFileFails(File data)
        throws Exception
    {
        assert data.exists();

        String code =
            "(with_ion_from_file " + printString(data.getPath()) +
                "  read)";

        expectAccessDenied("with_ion_from_file", data);
        sandbox().eval(code);
    }

    @Test
    public void testWithIonFromAbsoluteFileFails()
        throws Exception
    {
        withIonFromFileFails(HELLO_ABSOLUTE);
    }

    @Test
    public void testWithIonFromRelativeFileFails()
        throws Exception
    {
        withIonFromFileFails(HELLO_RELATIVE);
    }


    private void loadFails(File data)
        throws Exception
    {
        assert data.exists();

        TopLevel sandbox = sandbox();
        sandbox.requireModule("/fusion/eval");

        String code = "(load " + printString(data.getPath()) + ")";

        expectAccessDenied("load", data);
        sandbox.eval(code);
    }


    @Test
    public void testAbsoluteLoadFails()
        throws Exception
    {
        loadFails(HELLO_ABSOLUTE);
    }

    @Test
    public void testRelativeLoadFails()
        throws Exception
    {
        loadFails(HELLO_RELATIVE);
    }

    @Test
    public void testTopLevelLoadFails()
        throws FusionException
    {
        expectAccessDenied("load", HELLO_RELATIVE);
        sandbox().load(HELLO_RELATIVE);
    }

    @Test
    public void testCurrentNamespaceIsSelf()
        throws Exception
    {
        TopLevel sandbox = sandbox();
        sandbox.requireModule("/fusion/eval");

        sandbox.eval("(define in_sandbox 1912)");

        // Invoke `eval` in a few ways to check various entry points.

        checkLong(1912, sandbox.eval("(eval in_sandbox)"));

        Object evalProc = sandbox.lookup("eval");
        Object varSym   = eval("(quote in_sandbox)");
        checkLong(1912, sandbox.call(evalProc, varSym));
        checkLong(1912, sandbox.call("eval", varSym));
    }


    @Test
    public void testStdinIsRedirected()
        throws FusionException
    {
        // TODO This test should insure that there's data available on stdin
        //   so it's clear that the sandbox is restricted. But there's no easy
        //   way to do that now, since {@code CurrentIonReaderParameter} is
        //   wired directly to System.in and we can't affect that from here.
        //   Since the JRE's stdin is open but empty while running tests, the
        //   call to `read` here hangs awaiting data.
        if (false)
        {
            supplyInput("123 456");
            checkLong(123, topLevel().call("read"));
        }

        TopLevel sandbox = sandbox();
        assertSame(topLevel().lookup("eof"),
                   sandbox.call("read"));
    }


    @Test
    public void testStdoutIsRedirected()
        throws FusionException
    {
        topLevel().eval("(display 123)");
        assertEquals("123", stdoutToString());

        sandbox().eval("(display 456)");
        assertEquals("123", stdoutToString());
    }

    @Test(expected = ClassCastException.class)
    public void testFaultyCallToUnsafeProc()
        throws FusionException
    {
        TopLevel sandbox = sandbox();
        sandbox.requireModule("/fusion/unsafe/list");

        sandbox.call("unsafe_list_element", 0, 0);
    }
}
