module.exports = {
  purge: {
      content: [
        "../**/*.html.eex",
        "../**/*.html.leex",
        "../**/views/**/*.ex",
        "../**/live/**/*.ex",
        "./js/**/*.js",
        './**/*.elm',
      ],
      // Dunno if we need this or not.
      options: {
        safelist: [/translate-x-*-squares/]
      }
  },
  darkMode: 'media', // or 'media' or 'class' or false
  theme: {
    extend: {
      gridTemplateRows: {
        // Simple 8 row grid
        '8': 'repeat(8, minmax(0, 1fr))',
      },
      gridTemplateCols: {
        // Simple 8 row grid
        '8': 'repeat(8, minmax(0, 1fr))',
      },
      width: {
        '1/8': '12.5%',
        'constrained-1/4': '25vw',
        'constrained-40%': '40vw',
        'constrained-1/2': '50vw',
        'constrained-1': '100vw'
      },
      height: {
        '1/8': '12.5%',
        'constrained-1/4': '25vw',
        'constrained-40%': '40vw',
        'constrained-1/2': '50vw',
        'constrained-1': '100vw'
      },
      translate: {
        '0-squares': '0%',
        '1-squares': '100%',
        '2-squares': '200%',
        '3-squares': '300%',
        '4-squares': '400%',
        '5-squares': '500%',
        '6-squares': '600%',
        '7-squares': '700%',
        'neg-0-squares': '0%',
        'neg-1-squares': '-100%',
        'neg-2-squares': '-200%',
        'neg-3-squares': '-300%',
        'neg-4-squares': '-400%',
        'neg-5-squares': '-500%',
        'neg-6-squares': '-600%',
        'neg-7-squares': '-700%',
      }
    }
  },
  variants: {
    extend: {},
  },
  plugins: [
//    require('@tailwindcss/forms')
  ],
}
