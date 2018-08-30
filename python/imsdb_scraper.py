import scrapy


class Scripts(scrapy.Spider):
    name = "script_spider"
    start_urls = ['http://www.imsdb.com/']

    def parse(self, response):
        ALPHABET = './/table[tr/td/text() = "Alphabetical"]/tr[position() > 1]/td/a/@href'
        for letter in response.selector.xpath(ALPHABET).extract():
            yield scrapy.Request(response.urljoin(letter),
                                 callback=self.parseletter)

    def parseletter(self, response):
        MOVIE = './/td[contains(h1, "Movie Scripts")]/p/a/@href'
        for movie in response.selector.xpath(MOVIE).extract():
            yield scrapy.Request(response.urljoin(movie),
                                 callback=self.gotoscript)

    def gotoscript(self, response):
        READSCRIPT = './/a[contains(text(), "Read") and contains(text(), "Script")]/@href'
        scriptpage = response.selector.xpath(READSCRIPT).extract_first()
        if scriptpage:
            yield scrapy.Request(response.urljoin(scriptpage),
                                 callback=self.parsescriptpage)

    def parsescriptpage(self, response):
        THESCRIPT = './/pre'
        script = response.selector.xpath(THESCRIPT).extract_first()
        if script:
            yield { 'script': script}
