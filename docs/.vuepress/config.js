module.exports = {
    title: 'Documentation GLaDOS',
    description: 'Documentation pour le compilateur GLaDOS.',
    themeConfig: {
        nav: [
            { text: 'Accueil', link: '/' },
            { text: 'Introduction', link: '/introduction/' },
            { text: 'Utilisation', link: '/utilisation/installation' },
            { text: 'Composants', link: '/composants/Main' },
            { text: 'Tests', link: '/tests/introduction' }
        ],
        sidebar: {
            '/introduction/': [
                ''
            ],
            '/utilisation/': [
                'installation',
                'configuration'
            ],
            '/composants/': [
                'Main',
                'ASTConversion',
                'DataByteCode',
                // ... autres fichiers de src
            ],
            '/tests/': [
                'introduction',
                'test1.lisp'
            ]
        }
    }
}
