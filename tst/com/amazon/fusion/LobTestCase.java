// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionLob.copyLobBytes;
import static com.amazon.fusion.FusionLob.unsafeLobAnnotate;
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
        assertNull(copyLobBytes(top, lob));

        Object annotated = unsafeLobAnnotate(top, lob, EMPTY_STRING_ARRAY);
        assertSame(lob, annotated);

        annotated = unsafeLobAnnotate(top, lob, new String[]{ "ann" });
        assertNotSame(lob, annotated);
        checkLobType(lob);
        assertTrue(isAnyNull(top, annotated));
        assertNull(copyLobBytes(top, annotated));
    }


    @Test
    public void testMakeEmptyLob()
        throws Exception
    {
        TopLevel top = topLevel();

        Object lob = makeLob(EMPTY_BYTE_ARRAY);
        checkLobType(lob);
        assertFalse(isAnyNull(top, lob));
        assertArrayEquals(EMPTY_BYTE_ARRAY, copyLobBytes(top, lob));

        Object annotated = unsafeLobAnnotate(top, lob, EMPTY_STRING_ARRAY);
        assertSame(lob, annotated);

        annotated = unsafeLobAnnotate(top, lob, new String[]{ "ann" });
        assertNotSame(lob, annotated);
        checkLobType(lob);
        assertFalse(isAnyNull(top, annotated));
        assertArrayEquals(EMPTY_BYTE_ARRAY, copyLobBytes(top, annotated));
    }


    @Test
    public void testNontrivialCopyBytes()
        throws Exception
    {
        TopLevel top = topLevel();

        Object lob = makeLob(new byte[]{ 0, 1, 2 });
        checkLobType(lob);

        byte[] bytes = copyLobBytes(top, lob);
        assertArrayEquals(new byte[]{ 0, 1, 2 }, bytes);
        assertNotSame(bytes, copyLobBytes(top, lob));

        // Check that we've got an independent copy
        bytes[0] = 5;
        bytes = copyLobBytes(top, lob);
        assertArrayEquals(new byte[]{ 0, 1, 2 }, bytes);

        Object annotated = unsafeLobAnnotate(top, lob, EMPTY_STRING_ARRAY);
        assertSame(lob, annotated);

        annotated = unsafeLobAnnotate(top, lob, new String[]{ "ann" });
        checkLobType(annotated);
        bytes = copyLobBytes(top, lob);
        assertArrayEquals(new byte[]{ 0, 1, 2 }, bytes);

        // Check that we've got an independent copy
        bytes[0] = 5;
        bytes = copyLobBytes(top, lob);
        assertArrayEquals(new byte[]{ 0, 1, 2 }, bytes);
    }
}
