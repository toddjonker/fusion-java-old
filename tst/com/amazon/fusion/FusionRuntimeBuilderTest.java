// Copyright (c) 2012-2020 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionRuntimeBuilder.standard;
import static com.amazon.fusion.FusionString.unsafeStringToJavaString;
import static com.amazon.fusion.junit.Reflect.assertEqualProperties;
import static com.amazon.fusion.junit.Reflect.getterFor;
import static com.amazon.fusion.junit.Reflect.invoke;
import static com.amazon.fusion.junit.Reflect.setterFor;
import static com.amazon.fusion.junit.Reflect.witherFor;
import static org.hamcrest.Matchers.instanceOf;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.OutputStream;
import java.lang.reflect.Method;
import com.amazon.ion.IonCatalog;
import com.amazon.ion.system.SimpleCatalog;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;


public class FusionRuntimeBuilderTest
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


    private static void assertCopiesAreEqual(FusionRuntimeBuilder b)
    {
        assertEqualProperties(b, b.copy());
        assertEqualProperties(b, b.mutable());
        assertEqualProperties(b, b.immutable());
    }


    /**
     * Test combinations of get/set/with methods for a builder property.
     */
    private <T> void changeProperty(FusionRuntimeBuilder orig,
                                    String property,
                                    Class<T> type,
                                    T newValue,
                                    T expectedNormalizedValue)
    {
        Method getter = getterFor(orig, property, type);
        Method setter = setterFor(orig, property, type);
        Method wither = witherFor(orig, property, type);

        Object defaultValue = invoke(orig, getter);

        FusionRuntimeBuilder b = orig.copy();
        invoke(b, setter, newValue);
        assertEquals(expectedNormalizedValue, invoke(b, getter));
        assertCopiesAreEqual(b);

        FusionRuntimeBuilder c = orig.copy();
        assertSame(c, invoke(c, wither, newValue));
        assertEqualProperties(b, c);
        assertCopiesAreEqual(c);

        // Immutable builder is copied on modification
        FusionRuntimeBuilder i = orig.copy().immutable();
        FusionRuntimeBuilder d = invoke(i, wither, newValue);
        assertNotSame(i, d);
        assertEqualProperties(orig, i); // the immutable hasn't changed
        assertEqualProperties(b, d);
        assertCopiesAreEqual(d);

        invoke(b, setter, defaultValue);
        assertEquals(defaultValue, invoke(b, getter));
        assertEqualProperties(orig, b);
        assertCopiesAreEqual(b);

        invoke(c, wither, defaultValue);
        assertEquals(defaultValue, invoke(c, getter));
        assertEqualProperties(orig, c);
        assertCopiesAreEqual(c);
    }


    //========================================================================


    @Test
    public void testDefaultBuilder()
    {
        FusionRuntimeBuilder b = standard();
        assertSame(b, b.mutable());

        assertEquals("/fusion", b.getDefaultLanguage());
        assertNull(b.getInitialCurrentOutputPort());
        assertNull(b.getInitialCurrentDirectory());
        assertNull(b.getBootstrapRepository());
        assertNull(b.getCoverageDataDirectory());
        assertNull(b.getCoverageCollector());
        assertFalse(b.isDocumenting());

        assertCopiesAreEqual(b);
    }


    //========================================================================


    @Test
    public void testSetDefaultLanguage()
    {
        changeProperty(standard(), "DefaultLanguage", String.class,
                       "/fusion/base", "/fusion/base");
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
    public void testDefaultLanguageImmutability()
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


    private void changeDefaultIonCatalog(FusionRuntimeBuilder orig,
                                         IonCatalog catalog)
    {
        changeProperty(orig, "DefaultIonCatalog", IonCatalog.class,
                       catalog, catalog);
    }


    @Test
    public void testSetDefaultIonCatalog()
    {
        IonCatalog catalog = new SimpleCatalog();

        changeDefaultIonCatalog(standard(), catalog);
    }

    @Test
    public void testDefaultIonCatalogImmutability()
    {
        thrown.expect(UnsupportedOperationException.class);
        standard().immutable().setDefaultIonCatalog(new SimpleCatalog());
    }

    @Test
    public void testBuildingDefaultIonCatalog()
        throws FusionException
    {
        FusionRuntimeBuilder b = standard();
        FusionRuntime r = build(b);
        assertThat(r.getDefaultIonCatalog(), instanceOf(SimpleCatalog.class));

        SimpleCatalog catalog = new SimpleCatalog();
        b.withDefaultIonCatalog(catalog);
        r = build(b);
        assertSame(catalog, r.getDefaultIonCatalog());
    }


    //========================================================================


    private void changeInitialCurrentOutputPort(FusionRuntimeBuilder orig,
                                                OutputStream out)
    {
        changeProperty(orig, "InitialCurrentOutputPort", OutputStream.class,
                       out, out);
    }


    @Test
    public void testSetInitialCurrentOutputPort()
    {
        OutputStream out = new ByteArrayOutputStream();
        changeInitialCurrentOutputPort(standard(), out);
    }


    private void checkCurrentOutputPort(OutputStream expected,
                                        FusionRuntimeBuilder b)
        throws FusionException
    {
        TopLevel t = build(b).makeTopLevel(GlobalState.KERNEL_MODULE_NAME);
        Object actual = t.eval("(current_output_port)");
        assertSame(expected, actual);
    }

    @Test
    public void testBuildingInitialCurrentOutputPort()
        throws Exception
    {
        FusionRuntimeBuilder b = standard();
        checkCurrentOutputPort(System.out, b);

        OutputStream out = new ByteArrayOutputStream();
        b.setInitialCurrentOutputPort(out);
        checkCurrentOutputPort(out, b);
    }

    @Test
    public void testInitialCurrentOutputPortImmutability()
    {
        ByteArrayOutputStream out = new ByteArrayOutputStream();

        thrown.expect(UnsupportedOperationException.class);
        standard().immutable().setInitialCurrentOutputPort(out);
    }



    //========================================================================


    private void changeInitialCurrentDirectory(FusionRuntimeBuilder orig,
                                               File dir)
    {
        changeProperty(orig, "InitialCurrentDirectory", File.class,
                       dir, dir.getAbsoluteFile());
    }


    @Test
    public void testSetInitialCurrentDirectory()
    {
        File dir = new File("src");
        assertTrue(dir.isDirectory());

        changeInitialCurrentDirectory(standard(), dir);
    }


    private void checkCurrentDirectory(String expectedDir, FusionRuntimeBuilder b)
        throws FusionException
    {
        TopLevel t = build(b).getDefaultTopLevel();
        t.requireModule("/fusion/io");
        Object actualDir = t.eval("(current_directory)");
        assertEquals(expectedDir, unsafeStringToJavaString(t, actualDir));
    }

    @Test
    public void testBuildingInitialCurrentDirectory()
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


    //========================================================================


    private void changeBootstrapRepository(FusionRuntimeBuilder orig, File dir)
    {
        changeProperty(orig, "BootstrapRepository", File.class,
                       dir, dir.getAbsoluteFile());
    }


    @Test
    public void testSetBootstrapRepository()
    {
        File dir = new File("fusion");
        assertTrue(dir.isDirectory());

        changeBootstrapRepository(standard(), dir);
    }


    @Test
    public void testBootstrapRepositoryDoesNotExist()
    {
        File file = new File("no-such-file");
        assertFalse(file.exists());

        thrown.expect(IllegalArgumentException.class);
        standard().setBootstrapRepository(file);
    }

    @Test
    public void testBootstrapRepositoryIsNotValid()
    {
        // This is a Fusion repo, but not a bootstrap repository.
        File file = new File("ftst/repo");

        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage("Not a Fusion bootstrap repository: ");
        standard().setBootstrapRepository(file);
    }

    @Test
    public void testBootstrapRepositoryIsNormalFile()
    {
        File file = new File("build.xml");
        assertTrue(file.isFile());

        thrown.expect(IllegalArgumentException.class);
        standard().setBootstrapRepository(file);
    }

    @Test
    public void testBootstrapRepositoryImmutability()
    {
        thrown.expect(UnsupportedOperationException.class);
        standard().immutable().setBootstrapRepository(TEST_REPO);
    }


    //========================================================================


    private void changeCoverageDataDirectory(FusionRuntimeBuilder orig,
                                             File dir)
    {
        changeProperty(orig, "CoverageDataDirectory", File.class,
                       dir, dir.getAbsoluteFile());
    }


    @Test
    public void testSetCoverageDataDirectory()
    {
        File dir = new File("src");
        assertTrue(dir.isDirectory());

        changeCoverageDataDirectory(standard(), dir);
    }


    @Test
    public void testCoverageDataDirectoryDoesNotExist()
    {
        File file = new File("no-such-file");
        assertFalse(file.exists());

        // Directory doesn't have to exist
        changeCoverageDataDirectory(standard(), file);
    }


    @Test
    public void testCoverageDataDirectoryIsNormalFile()
    {
        File file = new File("build.xml");
        assertTrue(file.isFile());

        thrown.expect(IllegalArgumentException.class);
        standard().setCoverageDataDirectory(file);
    }


    @Test
    public void testCoverageDataDirectoryImmutability()
    {
        thrown.expect(UnsupportedOperationException.class);
        standard().immutable().setCoverageDataDirectory(TEST_REPO);
    }


    //========================================================================


    @Test
    public void testSetDocumenting()
    {
        FusionRuntimeBuilder std = standard();

        FusionRuntimeBuilder b = std.copy();
        b.setDocumenting(true);
        assertTrue(b.isDocumenting());
        assertCopiesAreEqual(b);
    }

    @Test
    public void testDocumentingImmutability()
    {
        thrown.expect(UnsupportedOperationException.class);
        standard().immutable().setDocumenting(true);
    }

}
