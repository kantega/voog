const path = require("path");
const merge = require("webpack-merge");
const elmMinify = require("elm-minify");

const MODE = process.env.npm_lifecycle_event === 'build' ? 'production' : 'development';

const common = {
    mode: MODE,

    entry: {
        voog: [
            './src/voog.js'
        ],
    },

    output: {
        path: path.resolve(__dirname + '/dist'),
        filename: '[name].js',
        libraryTarget: 'umd',
    },

    module: {
        rules: [
            {
                test: /\.(css|scss)$/,
                use: [
                    'style-loader',
                    'css-loader',
                ]
            },
            {
                test: /\.html$/,
                exclude: /node_modules/,
                loader: 'file-loader',
                options: {
                    name: '[name].[ext]',
                },
            },
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                loader: 'elm-webpack-loader',
                options: {
                    optimize: true,
                },
            },
            {
                test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                loader: 'url-loader',
                options: {
                    limit: 10000,
                    mimetype: 'application/font-woff'
                },
            },
            {
                test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                loader: 'file-loader',
            },
        ]
    },

    plugins: [
        new elmMinify.WebpackPlugin(),
    ],
};

if (MODE === "development") {
    module.exports = merge(common, {
        entry: {
            index: [
                './src/index.js'
            ],
        },

        devServer: {
            inline: true,
            stats: {colors: true},
        },
    });
} else {
    module.exports = common;
}
