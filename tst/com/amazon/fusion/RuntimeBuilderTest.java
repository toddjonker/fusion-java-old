// Copyright (c) 2012-2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionRuntimeBuilder.standard;
import static com.amazon.fusion.FusionString.unsafeStringToJavaString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.File;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;


public class RuntimeBuilderTest
{
    private static final File TEST_REPO = new File("tst-repo");

    @Rule
    public ExpectedException thrown = ExpectedException.none();


    private FusionRuntime build(FusionRuntimeBuilder b)
        throws FusionException
    {
        b = b.withBootstrapRepository(new File("fusion"));
        return b.build();
    }


    @Test
    public void testDefaultBuilder()
    {
        FusionRuntimeBuilder b = FusionRuntimeBuilder.standard();
        assertSame(b, b.mutable());

        assertEquals("/fusion", b.getDefaultLanguage());
        assertEquals("/fusion", b.copy().getDefaultLanguage());
        assertEquals("/fusion", b.immutable().getDefaultLanguage());
    }


    //========================================================================


    @Test
    public void testSetDefaultLanguage()
    {
        FusionRuntimeBuilder b = standard();
        b.setDefaultLanguage("/fusion/base");

        assertEquals("/fusion/base", b.getDefaultLanguage());
        assertEquals("/fusion/base", b.copy().getDefaultLanguage());
        assertEquals("/fusion/base", b.immutable().getDefaultLanguage());

        b = b.immutable().withDefaultLanguage("/fusion");
        assertEquals("/fusion", b.getDefaultLanguage());
    }

    @Test
    public void testNullDefaultLanguage()
    {
        thrown.expect(IllegalArgumentException.class);
        standard().setDefaultLanguage(null);
    }

    @Test
    public void testEmptyDefaultLanguage()
    {
        thrown.expect(IllegalArgumentException.class);
        standard().setDefaultLanguage("");
    }

    @Test
    public void testMalformedDefaultLanguage()
    {
        thrown.expect(IllegalArgumentException.class);
        standard().setDefaultLanguage("fusion");
    }

    @Test
    public void testDefaultLanguageImmutablity()
    {
        thrown.expect(UnsupportedOperationException.class);
        standard().immutable().setDefaultLanguage("/fusion/base");
    }


    @Test
    public void testBuildingDefaultLanguage()
        throws Exception
    {
        FusionRuntimeBuilder b = standard();
        FusionRuntime r = build(b);
        assertEquals("/fusion", r.getDefaultLanguage());

        // Change the default
        b.setDefaultLanguage("/fusion/base");
        r = build(b);
        assertEquals("/fusion/base", r.getDefaultLanguage());

        // Test effect by looking for something in /fusion but not /fusion/base
        thrown.expect(UnboundIdentifierException.class);
        r.getDefaultTopLevel().eval("always");
    }


    //========================================================================
    

    private void checkCurrentDirectory(String expectedDir, FusionRuntimeBuilder b)
        throws FusionException
    {
        TopLevel t = build(b).getDefaultTopLevel();
        t.requireModule("/fusion/io");
        Object actualDir = t.eval("(current_directory)");
        assertEquals(expectedDir, unsafeStringToJavaString(t, actualDir));
    }

    @Test
    public void testDefaultCurrentDirectory()
        throws Exception
    {
        FusionRuntimeBuilder b = standard();
        checkCurrentDirectory(System.getProperty("user.dir"), b);

        File tst = new File("tst");
        b.setInitialCurrentDirectory(tst);
        checkCurrentDirectory(tst.getAbsolutePath(), b);
    }


    @Test
    public void testCurrentDirectoryDoesNotExist()
    {
        File file = new File("no-such-file");
        assertFalse(file.exists());
        thrown.expect(IllegalArgumentException.class);
        standard().setInitialCurrentDirectory(file);
    }


    @Test
    public void testCurrentDirectoryIsNormalFile()
    {
        File file = new File("build.xml");
        assertTrue(file.isFile());

        thrown.expect(IllegalArgumentException.class);
        standard().setInitialCurrentDirectory(file);
    }

    @Test
    public void testInitialCurrentDirectoryImmutability()
    {
        thrown.expect(UnsupportedOperationException.class);
        standard().immutable().setInitialCurrentDirectory(TEST_REPO);
    }


    @Test
    public void testDocumentingImmutability()
    {
        thrown.expect(UnsupportedOperationException.class);
        standard().immutable().setDocumenting(true);
    }

}
