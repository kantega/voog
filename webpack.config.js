const path = require("path");
const elmMinify = require("elm-minify");

module.exports = {
  mode: process.env.npm_lifecycle_event === 'build' ? 'production' : 'development',

  entry: {
    voog: [
      './src/voog.js'
    ],
    index: [
      './src/index.js'
    ],
  },

  output: {
    path: path.resolve(__dirname + '/dist'),
    filename: '[name].js',
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
        test:    /\.html$/,
        exclude: /node_modules/,
        loader:  'file-loader',
        options: {
          name: '[name].[ext]',
        },
      },
      {
        test:    /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader:  'elm-webpack-loader',
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

  devServer: {
    inline: true,
    stats: { colors: true },
  },
};
