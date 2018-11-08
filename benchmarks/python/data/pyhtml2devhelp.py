#! /usr/bin/python3

from html.parser import HTMLParser
from xml.sax.saxutils import escape
import os, sys, re

class PyHTMLParser(HTMLParser):
    pages_to_include = set(('whatsnew/index.html', 'tutorial/index.html', 'using/index.html',
                            'reference/index.html', 'library/index.html', 'howto/index.html',
                            'extending/index.html', 'c-api/index.html', 'install/index.html',
                            'distutils/index.html'))

    def __init__(self, basedir, fn, indent, parents=set()):
        HTMLParser.__init__(self, convert_charrefs=True)
        self.basedir = basedir
        self.dir, self.fn = os.path.split(fn)
        self.data = ''
        self.parents = parents
        self.link = {}
        self.indent = indent
        self.last_indent = indent - 1
        self.sub_indent = 0
        self.sub_count = 0
        self.next_link = False

    def escape(self, text):
        return escape(text, {'"': '&quot;'})

    def process_link(self):
        new_href = self.escape(os.path.join(self.dir, self.link['href']))
        text = self.escape(self.link['text'])
        indent = self.indent + self.sub_indent
        if self.last_indent == indent:
            print('%s</sub>' % ('  ' * self.last_indent))
            self.sub_count -= 1
        print('%s<sub link="%s" name="%s">' % ('  ' * indent, new_href, text))
        self.sub_count += 1
        self.last_indent = self.indent + self.sub_indent

    def handle_starttag(self, tag, attrs):
        if tag == 'a':
            self.start_a(attrs)
        elif tag == 'li':
            self.start_li(attrs)

    def handle_endtag(self, tag):
        if tag == 'a':
            self.end_a()
        elif tag == 'li':
            self.end_li()

    def start_li(self, attrs):
        self.sub_indent += 1
        self.next_link = True

    def end_li(self):
        indent = self.indent + self.sub_indent
        if self.sub_count > 0:
            print('%s</sub>' % ('  ' * self.last_indent))
            self.sub_count -= 1
            self.last_indent -= 1
        self.sub_indent -= 1

    def start_a(self, attrs):
        self.link = {}
        for attr in attrs:
            self.link[attr[0]] = attr[1]
        self.data = ''
        
    def end_a(self):
        process = False
        text = self.escape(self.data.replace('\t', '').replace('\n', ' '))
        self.link['text'] = text
        # handle a tag without href attribute
        try:
            href = self.link['href']
        except KeyError:
            return

        abs_href = os.path.join(self.basedir, href)
        if abs_href in self.parents:
            return
        if href.startswith('..') or href.startswith('http:') \
               or href.startswith('mailto:') or href.startswith('news:'):
            return
        if href in ('', 'about.html', 'modindex.html', 'genindex.html', 'glossary.html',
                    'search.html', 'contents.html', 'download.html', 'bugs.html',
                    'license.html', 'copyright.html'):
            return

        if 'class' in self.link:
            if self.link['class'] in ('biglink'):
                process = True
            if self.link['class'] in ('reference external'):
                if self.next_link:
                    process = True
                    next_link = False

        if process == True:
            self.process_link()
            if href in self.pages_to_include:
                self.parse_file(os.path.join(self.dir, href))

    def finish(self):
        if self.sub_count > 0:
            print('%s</sub>' % ('  ' * self.last_indent))

    def handle_data(self, data):
        self.data += data

    def parse_file(self, href):
        # TODO basedir bestimmen
        parent = os.path.join(self.basedir, self.fn)
        self.parents.add(parent)
        parser = PyHTMLParser(self.basedir, href, self.indent + 1,
                              self.parents)
        text = open(self.basedir + '/' + href, encoding='latin_1').read()
        parser.feed(text)
        parser.finish()
        parser.close()
        if parent in self.parents:
            self.parents.remove(parent)

class PyIdxHTMLParser(HTMLParser):
    def __init__(self, basedir, fn, indent):
        HTMLParser.__init__(self, convert_charrefs=True)
        self.basedir = basedir
        self.dir, self.fn = os.path.split(fn)
        self.data = ''
        self.link = {}
        self.indent = indent
        self.active = False
        self.indented = False
        self.nolink = False
        self.header = ''
        self.last_letter = 'Z'
        self.last_text = ''

    def escape(self, text):
        return escape(text, {'"': '&quot;'})

    def process_link(self):
        new_href = self.escape(os.path.join(self.dir, self.link['href']))
        text = self.escape(self.link['text'])
        if not self.active:
            return
        if text.startswith('['):
            return
        if self.link.get('rel', None) in ('prev', 'parent', 'next', 'contents', 'index'):
            return
        if self.indented:
            text = self.last_text + ' ' + text
        else:
            # Save it in case we need it again
            self.last_text = re.sub(' \([\w\-\.\s]+\)', '', text)
        indent = self.indent
        print('%s<function link="%s" name="%s"/>' % ('  ' * indent, new_href, text))

    def handle_starttag(self, tag, attrs):
        if tag == 'a':
            self.start_a(attrs)
        elif tag == 'dl':
            self.start_dl(attrs)
        elif tag == 'dt':
            self.start_dt(attrs)
        elif tag == 'h2':
            self.start_h2(attrs)
        elif tag == 'td':
            self.start_td(attrs)
        elif tag == 'table':
            self.start_table(attrs)

    def handle_endtag(self, tag):
        if tag == 'a':
            self.end_a()
        elif tag == 'dl':
            self.end_dl()
        elif tag == 'dt':
            self.end_dt()
        elif tag == 'h2':
            self.end_h2()
        elif tag == 'td':
            self.end_td()
        elif tag == 'table':
            self.end_table()

    def start_dl(self, attrs):
        if self.last_text:
            # Looks like we found the second part to a command
            self.indented = True

    def end_dl(self):
        self.indented = False

    def start_dt(self, attrs):
        self.data = ''
        self.nolink = True

    def end_dt(self):
        if not self.active:
            return
        if self.nolink == True:
            # Looks like we found the first part to a command
            self.last_text = re.sub(' \([\w\-\.\s]+\)', '', self.data)
            self.nolink = False

    def start_h2(self, attrs):
        for k, v in attrs:
            if k == 'id':
                self.header = v
                if v == '_':
                    self.active = True

    def end_h2(self):
        pass

    def start_td(self, attrs):
        self.indented = False
        self.last_text = ''

    def end_td(self):
        pass

    def start_table(self, attrs):
        pass

    def end_table(self):
        if self.header == self.last_letter:
            self.active = False

    def start_a(self, attrs):
        self.nolink = False
        self.link = {}
        for attr in attrs:
            self.link[attr[0]] = attr[1]
        self.data = ''
        
    def end_a(self):
        text = self.data.replace('\t', '').replace('\n', ' ')
        text = text.replace("Whats ", "What's ")
        self.link['text'] = text
        # handle a tag without href attribute
        try:
            href = self.link['href']
        except KeyError:
            return
        self.process_link()

    def handle_data(self, data):
        self.data += data

    def handle_entityref(self, name):
        # not meant to be called while convert_charrefs is true
        raise AssertionError('entityrefs should not be handled any more')

def main():
    base = sys.argv[1]
    fn = sys.argv[2]
    version = escape(sys.argv[3])

    parser = PyHTMLParser(base, fn, indent=0)
    print('<?xml version="1.0" encoding="iso-8859-1"?>')
    print('<book title="Python %s Documentation" name="Python %s" version="%s" link="index.html">' % (version, version, version))
    print('<chapters>')
    parser.parse_file(fn)
    print('</chapters>')

    print('<functions>')

    fn = 'genindex-all.html'
    parser = PyIdxHTMLParser(base, fn, indent=1)
    text = open(base + '/' + fn, encoding='latin_1').read()
    parser.feed(text)
    parser.close()

    print('</functions>')
    print('</book>')

main()
