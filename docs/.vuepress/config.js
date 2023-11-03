module.exports = {
    title: 'Documentation GLaDOS',
    description: 'Documentation pour le compilateur GLaDOS.',
    themeConfig: {
        nav: [
            { text: 'Accueil', link: '/' },
            { text: 'Introduction', link: '/introduction/' },
            { text: 'Utilisation', link: '/utilisation/configuration' },
            { text: 'Composants', link: '/composants/' },
            { text: 'Tests', link: '/tests/introduction' }
        ],
        sidebar: {
            '/introduction/': [
                ''
            ],
            '/utilisation/': [
                'configuration'
            ],
            '/composants/': [
                'Main',
                'ASTConversion',
                'DataByteCode',
                'Datas',
                'EvalByteCode',
                'ParseAnd',
                'ParseChar',
                'ParseExpr',
                'ParseOr',
                'ParserModule',
                'ParseUtils',
                'Types',
                'WriteByteCode',
            ],
            '/tests/': [
                'introduction',
                'test1.lisp'
            ]
        }
    }
}
