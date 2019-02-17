// Copyright (c) 2012-2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.File;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;


public class RuntimeBuilderTest
    extends CoreTestCase
{
    private static final File TEST_REPO = new File("tst-repo");

    @Rule
    public ExpectedException thrown = ExpectedException.none();


    @Test
    public void testDefaultBuilder()
        throws Exception
    {
        FusionRuntimeBuilder b = FusionRuntimeBuilder.standard();
        assertSame(b, b.mutable());

        assertEquals("/fusion", b.getDefaultLanguage());
        assertEquals("/fusion", b.copy().getDefaultLanguage());
        assertEquals("/fusion", b.immutable().getDefaultLanguage());
    }


    @Test
    public void testSetDefaultLanguage()
        throws Exception
    {
        FusionRuntimeBuilder b = runtimeBuilder();
        b.setDefaultLanguage("/fusion/base");
        assertEquals("/fusion/base", runtime().getDefaultLanguage());
        expectUnboundIdentifierExn("always");

        assertEquals("/fusion/base", b.getDefaultLanguage());
        assertEquals("/fusion/base", b.copy().getDefaultLanguage());
        assertEquals("/fusion/base", b.immutable().getDefaultLanguage());

        b = b.immutable().withDefaultLanguage("/fusion");
        assertEquals("/fusion", b.getDefaultLanguage());
    }

    @Test(expected = IllegalArgumentException.class)
    public void testNullDefaultLanguage()
        throws Exception
    {
        runtimeBuilder().setDefaultLanguage(null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testEmptyDefaultLanguage()
        throws Exception
    {
        runtimeBuilder().setDefaultLanguage("");
    }

    @Test(expected = IllegalArgumentException.class)
    public void testMalformedDefaultLanguage()
        throws Exception
    {
        runtimeBuilder().setDefaultLanguage("fusion");
    }

    @Test(expected = UnsupportedOperationException.class)
    public void testDefaultLanguageImmutablity()
        throws Exception
    {
        runtimeBuilder().immutable().setDefaultLanguage("/fusion/base");
    }


    @Test
    public void testDefaultCurrentDirectory()
        throws Exception
    {
        topLevel().requireModule("/fusion/io");
        assertString(System.getProperty("user.dir"), "(current_directory)");
    }


    @Test(expected = IllegalArgumentException.class)
    public void testCurrentDirectoryDoesNotExist()
        throws Exception
    {
        File file = new File("no-such-file");
        assertFalse(file.exists());
        runtimeBuilder().setInitialCurrentDirectory(file);
    }


    @Test(expected = IllegalArgumentException.class)
    public void testCurrentDirectoryIsNormalFile()
        throws Exception
    {
        File file = new File("build.xml");
        assertTrue(file.isFile());
        runtimeBuilder().setInitialCurrentDirectory(file);
    }

    @Test(expected = UnsupportedOperationException.class)
    public void testInitialCurrentDirectoryImmutability()
        throws Exception
    {
        runtimeBuilder().immutable().setInitialCurrentDirectory(TEST_REPO);
    }


    @Test(expected = UnsupportedOperationException.class)
    public void testDocumentingImmutability()
        throws Exception
    {
        runtimeBuilder().immutable().setDocumenting(true);
    }

}
