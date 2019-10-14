module.exports = {
  extends: [
    //'eslint:recommended',
    //'plugin:react/recommended',
    //'plugin:@typescript-eslint/recommended',
    //1'plugin:jest/recommended',
    //'plugin:import/errors',
    //'plugin:import/warnings',
  ],
  env: {
    browser: true,
    node: true,
    es6: true,
  },
  parser: '@typescript-eslint/parser',
  parserOptions: {
    ecmaVersion: 2018,
    sourceType: 'module',
  },
  plugins: [
    //'react',
    //'react-hooks',
    //'import',
    //'@typescript-eslint/eslint-plugin',
  ],
  rules: {
    'import/no-unresolved': ['error', { commonjs: true, caseSensitive: true }],
    'import/order': [
      'error',
      { groups: [['builtin', 'external', 'internal']] },
    ],
    'import/newline-after-import': 'error',
    '@typescript-eslint/indent': ['error', 2],
    '@typescript-eslint/prefer-interface': 'off',
    '@typescript-eslint/explicit-member-accessibility': 'off',
    '@typescript-eslint/explicit-function-return-type': 'off',
    '@typescript-eslint/no-unused-vars': [
      'error',
      {
        args: 'after-used',
      },
    ],
    '@typescript-eslint/indent': 'off',
    'jsx-quotes': ['error', 'prefer-double'],
    '@typescript-eslint/camelcase': 'off',
  },
  settings: {
    //react: {
      //pragma: 'React',
      //version: 'detect',
    },
    'import/resolver': {
      node: {
        extensions: ['.js', '.jsx', '.ts', '.tsx'],
      },
    },
  },
};
