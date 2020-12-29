def tag(name, *content, cls=None, **attrs):
    """Generate HTML tags"""
    attrs_str = ''

    if cls is not None:
        attrs['class'] = cls
    if attrs:
        attrs_str = ''.join(' %s="%s"' % (attr, value)
                            for attr, value in sorted(attrs.items()))
    if content:
        return '\n'.join('<%s%s>%s</%s>' %
                         (name, attrs_str, c, name) for c in content)
    else:
        return '<%s%s />' % (name, attrs_str)


if __name__ == "__main__":
    print(tag('br'))
    print(tag('p', 'hello'))
    print(tag('div', 'button', id='button-canvas', cls='div1'))
    print(tag(content='testing', name='img'))
