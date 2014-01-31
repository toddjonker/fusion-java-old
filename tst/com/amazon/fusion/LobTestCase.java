// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionLob.unsafeLobAnnotate;
import static com.amazon.fusion.FusionLob.unsafeLobBytesCopy;
import static com.amazon.fusion.FusionLob.unsafeLobBytesNoCopy;
import static com.amazon.fusion.FusionUtils.EMPTY_BYTE_ARRAY;
import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import static com.amazon.fusion.FusionValue.isAnyNull;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 * Base class for blob/clob tests.
 */
public abstract class LobTestCase
    extends CoreTestCase
{
    /**
     * @param value may be null or empty
     */
    abstract Object makeLob(byte[] value)
        throws FusionException;

    abstract void checkLobType(Object lob)
        throws FusionException;


    @Test
    public void testMakeNullLob()
        throws Exception
    {
        TopLevel top = topLevel();

        Object lob = makeLob(null);
        checkLobType(lob);
        assertTrue(isAnyNull(top, lob));
        assertNull(unsafeLobBytesCopy(top, lob));
        assertNull(unsafeLobBytesNoCopy(top, lob));

        Object annotated = unsafeLobAnnotate(top, lob, EMPTY_STRING_ARRAY);
        assertSame(lob, annotated);

        annotated = unsafeLobAnnotate(top, lob, new String[]{ "ann" });
        assertNotSame(lob, annotated);
        checkLobType(lob);
        assertTrue(isAnyNull(top, annotated));
        assertNull(unsafeLobBytesCopy(top, annotated));
        assertNull(unsafeLobBytesNoCopy(top, annotated));
    }


    @Test
    public void testMakeEmptyLob()
        throws Exception
    {
        TopLevel top = topLevel();

        Object lob = makeLob(EMPTY_BYTE_ARRAY);
        checkLobType(lob);
        assertFalse(isAnyNull(top, lob));
        assertArrayEquals(EMPTY_BYTE_ARRAY, unsafeLobBytesCopy(top, lob));
        assertArrayEquals(EMPTY_BYTE_ARRAY, unsafeLobBytesNoCopy(top, lob));

        Object annotated = unsafeLobAnnotate(top, lob, EMPTY_STRING_ARRAY);
        assertSame(lob, annotated);

        annotated = unsafeLobAnnotate(top, lob, new String[]{ "ann" });
        assertNotSame(lob, annotated);
        checkLobType(lob);
        assertFalse(isAnyNull(top, annotated));
        assertArrayEquals(EMPTY_BYTE_ARRAY, unsafeLobBytesCopy(top, annotated));
        assertArrayEquals(EMPTY_BYTE_ARRAY, unsafeLobBytesNoCopy(top, annotated));
    }


    @Test
    public void testNontrivialCopyBytes()
        throws Exception
    {
        TopLevel top = topLevel();

        Object lob = makeLob(new byte[]{ 0, 1, 2 });
        checkLobType(lob);

        byte[] bytes = unsafeLobBytesCopy(top, lob);
        assertArrayEquals(new byte[]{ 0, 1, 2 }, bytes);
        assertNotSame(bytes, unsafeLobBytesCopy(top, lob));
        byte[] noCopy = unsafeLobBytesNoCopy(top, lob);
        assertArrayEquals(bytes, noCopy);
        assertNotSame(bytes, noCopy);

        // Check that we've got an independent copy
        bytes[0] = 5;
        assertArrayEquals(new byte[]{ 0, 1, 2 }, unsafeLobBytesNoCopy(top, lob));

        Object annotated = unsafeLobAnnotate(top, lob, EMPTY_STRING_ARRAY);
        assertSame(lob, annotated);

        annotated = unsafeLobAnnotate(top, lob, new String[]{ "ann" });
        checkLobType(annotated);
        bytes = unsafeLobBytesCopy(top, lob);
        assertArrayEquals(new byte[]{ 0, 1, 2 }, bytes);
        noCopy = unsafeLobBytesNoCopy(top, lob);
        assertArrayEquals(bytes, noCopy);
        assertNotSame(bytes, noCopy);

        // Check that we've got an independent copy
        bytes[0] = 5;
        assertArrayEquals(new byte[]{ 0, 1, 2 }, unsafeLobBytesNoCopy(top, lob));
    }
}
