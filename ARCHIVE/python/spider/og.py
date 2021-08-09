from html.parser import HTMLParser
from urllib.request import urlopen
from urllib import parse


class LinkParser(HTMLParser):

    def __init__(self):
        self.baseUrl = ''

    def handle_starttag(self, tag, attrs):
        if tag == 'a':
            for (key, value) in attrs:
                if key == 'href':
                    newUrl = parse.urljoin(self.baseUrl, value)
                    self.links = self.links + [newUrl]

    def getLinks(self, url):
        self.links = []
        self.baseUrl = url
        response = urlopen(url)
        if response.getheader('Content-Type') == 'text/html':
            htmlBytes = response.read()
            htmlString = htmlBytes.decode('utf-8')
            self.feed(htmlString)
            return htmlString, self.links
        else:
            return '', []


def spider(url, word, max_pages=100):
    pages_to_visits = [url]
    number_visited = 0
    found_word = False

    while number_visited < max_pages and pages_to_visits != [] and not found_word:
        number_visited += 1
        url = pages_to_visits[0]
        pages_to_visits = pages_to_visits[1:]
        try:
            print(number_visited, 'Visiting:', url)
            parser = LinkParser()
            data, links = parser.getLinks(url)
            if data.find(word) > -1:
                found_word = True
            pages_to_visits = pages_to_visits + links
            print(' ** Success! **')
        except Exception:
            print(' ** Failed! **')

    if found_word:
        print('The word', word, 'was found at', url)
    else:
        print('Word never found')
