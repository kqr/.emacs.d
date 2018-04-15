/*
 * Copyright (C) 2002, 2003 by Nick Sieger
 *
 * $Revision: 1.1 $
 * $Date: 2003/02/15 20:58:26 $
 *
 * Author: Nick Sieger <nsieger@bitstream.net>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package jde.juci;

import java.util.*;
import java.io.*;

import org.junit.*;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Test the {@link LispWriter} class.
 *
 * @author <a href="mailto:nsieger@bitstream.net">Nick Sieger</a>
 * @version 1.0
 */
public class LispWriterTest {

    private LispWriter lwriter;
    private StringWriter output;

    @Before
    public void setUp() {
        reset();
    }

    private void reset() {
        output = new StringWriter();
        lwriter = new LispWriter(new PrintWriter(output));
    }

    @Test
    public void testInt1() {
        lwriter.writeInt(1010);
        assertEquals("1010", output.toString());
        reset();
        lwriter.writeUnknown(1010);
        assertEquals("1010", output.toString());
    }

    @Test
    public void testDouble1() {
        lwriter.writeDouble(10.1d);
        assertEquals("10.1", output.toString());
        reset();
        lwriter.writeUnknown(10.1d);
        assertEquals("10.1", output.toString());
    }

    @Test
    public void testCons1() {
        lwriter.writeCons(new Cons("", new Symbol("find-buffer-file-type-coding-system")));
        assertEquals("(\"\" . find-buffer-file-type-coding-system)", output.toString());
        reset();
        lwriter.writeUnknown(new Cons("", new Symbol("find-buffer-file-type-coding-system")));
        assertEquals("(\"\" . find-buffer-file-type-coding-system)", output.toString());
    }

    @Test
    public void testMapAlist() {
        Map<Object,Object> m = new LinkedHashMap<>();
        m.put("foo", new Symbol("bar"));
        m.put("baz", new Symbol("quux"));
        lwriter.writeAlist(m);
        assertEquals("'((\"foo\" . bar) (\"baz\" . quux))", output.toString());
        reset();
        lwriter.writeUnknown(m);
        assertEquals("'((\"foo\" . bar) (\"baz\" . quux))", output.toString());
    }

    @Test
    public void testChars() {
        lwriter.writeChar('a');
        assertEquals("?a", output.toString());
        reset();
        lwriter.writeChar('?');
        assertEquals("?\\?", output.toString());
        reset();
        lwriter.writeChar('\\');
        assertEquals("?\\\\", output.toString());
    }

    @Test
    public void testForm1() {
        List l = Arrays.asList(
                new Symbol("message"), "Hello %s", new Symbol("user-full-name"));
        lwriter.writeForm(l);
        assertEquals("(message \"Hello %s\" user-full-name)", output.toString());
        reset();
        lwriter.writeUnknown(l);
        assertEquals("'(message \"Hello %s\" user-full-name)", output.toString());
    }

    @Test
    public void testQuoted1() {
        List l = new ArrayList();
        l.add(new Symbol("apply"));
        l.add(new Quoted(new Symbol("+")));
        l.add(1);
        l.add(2);
        List inner = new ArrayList();
        inner.add(3);
        inner.add(4);
        l.add(new Quoted(inner));
        lwriter.writeForm(l);
        assertEquals("(apply '+ 1 2 '(3 4))", output.toString());
        reset();
        lwriter.writeUnknown(l);
        assertEquals("'(apply '+ 1 2 '(3 4))", output.toString());
    }

    @Test
    public void testWriteJdeeJuciInvokeElispForm() {
        List eval = new ArrayList();
        eval.add(new Symbol("jdee-juci-invoke-elisp"));

        List form = new ArrayList();
        form.add(new Symbol("message"));
        form.addAll(Arrays.asList("hello %s", "nick"));

        eval.add(form);
        lwriter.writeForm(eval);
        assertEquals("(jdee-juci-invoke-elisp '(message \"hello %s\" \"nick\"))", output.toString());
    }

}
