// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.io.File;

/**
 * Identifies a source of Fusion code, a file, URL, <em>etc.</em>
 * The primary purpose of this class is to display a suitable message fragment
 * for error reporting to users.
 */
public class SourceName
{
    private final String myDisplay;

    /**
     * Creates a {@link SourceName} representing a file at the given path.
     *
     * @param path must not be null or empty.
     *
     * @see #forFile(File)
     */
    public static SourceName forFile(String path)
    {
        if (path.length() == 0) {
            throw new IllegalArgumentException("path must not be empty");
        }
        return new SourceName(path);
    }

    /**
     * Creates a {@link SourceName} representing a file.
     * The {@link File}'s absolute path will be displayed.
     *
     * @param path must not be null or empty.
     *
     * @see #forFile(File)
     */
    public static SourceName forFile(File path)
    {
        return new SourceName(path.getAbsolutePath());
    }

    /**
     * Creates a {@link SourceName} that will simply display the given text.
     * @param display must not be null.
     */
    public static SourceName forDisplay(String display)
    {
        if (display.length() == 0) {
            throw new IllegalArgumentException("display must not be empty");
        }
        return new SourceName(display);
    }


    /**
     * Not public yet. This should probably include the concrete location of
     * the module source, not just its identity.
     */
    static SourceName forModule(ModuleIdentity id)
    {
        // TODO identify may not be right for jar-based modules.
        return new SourceName(id.identify());
    }


    private SourceName(String display)
    {
        myDisplay = display;
    }


    /** Returns the human-readable source name, for display in messages. */
    String display()
    {
        return myDisplay;
    }
}
